-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, call/2, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HEAP_SIZE, 1000).
-define(PUT_STAT_TIMEOUT, 5 * 1000).

-record(state, {
  heap,
  reserved,
  stat_call_count,
  stat_reply_count,
  stat_reserve_count,
  stat_release_count,
  stat_retry_count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

call(RequesterPid, RequestData) -> gen_server:call(RequesterPid, {call, RequestData}, infinity).

reserve(RequesterPid, Count) -> gen_server:call(RequesterPid, {reserve, Count}, infinity).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  erlang:send_after(?PUT_STAT_TIMEOUT, self(), info_stat),

  {ok, #state{
    heap = [],
    reserved = #{},
    stat_call_count = 0,
    stat_reply_count = 0,
    stat_reserve_count = 0,
    stat_release_count = 0,
    stat_retry_count = 0
  }}.

%%%===================================================================
%%% call
%%%===================================================================

handle_call(
  {call, RequestData}, From,
  #state{heap = Heap, stat_call_count = StatCallCount} = State
) ->
  NewHeap = Heap ++ [{make_ref(), RequestData, From}],
  NewState = State#state{
    heap = NewHeap,
    stat_call_count = StatCallCount + 1
  },
  {noreply, NewState};

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call(
  {reserve, Count}, _From, #state{heap = Heap, reserved = Reserved, stat_reserve_count = StatReserveCount} = State
) when length(Heap) >= Count ->
  {ReservedHeap, NewHeap} = lists:split(Count, Heap),

  Result = [{RequestRef, RequestData} || {RequestRef, RequestData, _RequestFrom} <- ReservedHeap],
  ReservedMap = maps:from_list(
    [{RequestRef, {RequestData, RequestFrom}} || {RequestRef, RequestData, RequestFrom} <- ReservedHeap]
  ),

  NewReserved = maps:merge(Reserved, ReservedMap),
  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved,
    stat_reserve_count = StatReserveCount + Count
  },
  {reply, {ok, Result}, NewState};

handle_call({reserve, Count}, _From, #state{heap = Heap} = State) when length(Heap) < Count ->
  lager:warning("heap is empty"),
  {reply, {sleep, 1000}, State};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% result
%%%===================================================================

handle_cast(
  {release, RequestRef, {result, Result}},
  #state{reserved = Reserved, stat_reply_count = StatReplyCount, stat_release_count = StatReleaseCount} = State
) ->
  {{_RequestData, From}, NewReserved} = maps:take(RequestRef, Reserved),

  gen_server:reply(From, Result),

  NewState = State#state{
    reserved = NewReserved,
    stat_reply_count = StatReplyCount + 1,
    stat_release_count = StatReleaseCount + 1
  },
  {noreply, NewState};

%%%===================================================================
%%% retry
%%%===================================================================

handle_cast(
  {release, RequestRef, {retry, Reason}},
  #state{
    heap = Heap,
    reserved = Reserved,
    stat_release_count = StatReleaseCount,
    stat_retry_count = StatRetryCount
  } = State
) ->
  {{RequestData, From}, NewReserved} = maps:take(RequestRef, Reserved),

  lager:warning("retry ~p ~p", [RequestData, Reason]),

  NewHeap = [{RequestRef, RequestData, From} | Heap],
  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved,
    stat_release_count = StatReleaseCount + 1,
    stat_retry_count = StatRetryCount + 1
  },
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  info_stat,
  #state{
    heap = Heap,
    stat_call_count = StatCallCount,
    stat_reply_count = StatReplyCount,
    stat_reserve_count = StatReserveCount,
    stat_release_count = StatReleaseCount,
    stat_retry_count = StatRetryCount
  } = State
) ->
  erlang:send_after(?PUT_STAT_TIMEOUT, self(), info_stat),
  lager:info(
    "heap ~p call ~p reply ~p reserve ~p release ~p retry ~p",
    [length(Heap), StatCallCount, StatReplyCount, StatReserveCount, StatReleaseCount, StatRetryCount]
  ),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
