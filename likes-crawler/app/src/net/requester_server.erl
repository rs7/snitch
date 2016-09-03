-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PUT_STAT_TIMEOUT, 10 * 1000).

-record(state, {
  heap,
  reserved,
  wait_heap,
  stat_reserve_count,
  stat_release_count,
  stat_retry_count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

reserve(RequesterPid, Count) -> gen_server:call(RequesterPid, {reserve, Count}, infinity).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  erlang:send_after(?PUT_STAT_TIMEOUT, self(), put_stat),

  {ok, #state{
    heap = get_requests(1000),
    reserved = #{},
    wait_heap = [],
    stat_reserve_count = 0,
    stat_release_count = 0,
    stat_retry_count = 0
  }}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call(
  {reserve, Count}, _From,
  #state{heap = Heap, reserved = Reserved, stat_reserve_count = StatReserveCount} = State
) when length(Heap) >= Count ->
  {RequestInfos, NewHeap} = lists:split(Count, Heap),

  NewReserved = maps:merge(Reserved, maps:from_list(RequestInfos)),
  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved,
    stat_reserve_count = StatReserveCount + Count
  },
  Reply = {ok, RequestInfos},
  {reply, Reply, NewState};

handle_call(
  {reserve, Count}, From,
  #state{heap = Heap, wait_heap = WaitHeap} = State
) when length(Heap) < Count ->
  lager:warning("heap wait ~p ~p", [From, Count]),

  NewWaitHeap = [{From, Count} | WaitHeap],
  NewState = State#state{
    heap = NewWaitHeap
  },
  {noreply, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% result
%%%===================================================================

handle_cast(
  {release, RequestRef, {result, Result}},
  #state{reserved = Reserved, stat_release_count = StatReleaseCount} = State
) ->
  {RequestData, NewReserved} = maps:take(RequestRef, Reserved),
  RequestInfo = {RequestRef, RequestData},

  lager:debug("~p ~p", [RequestInfo, Result]),

  NewState = State#state{
    reserved = NewReserved,
    stat_release_count = StatReleaseCount + 1
  },
  {noreply, NewState};

%%%===================================================================
%%% retry
%%%===================================================================

handle_cast(
  {release, RequestRef, {retry, Reason}},
  #state{heap = Heap, reserved = Reserved, stat_release_count = StatReleaseCount, stat_retry_count = StatRetryCount} =
    State
) ->
  {RequestData, NewReserved} = maps:take(RequestRef, Reserved),
  RequestInfo = {RequestRef, RequestData},

  lager:warning("retry ~p ~p", [RequestInfo, Reason]),

  NewHeap = [RequestInfo | Heap],
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
  put_stat,
  #state{stat_reserve_count = StatReserveCount, stat_release_count = StatReleaseCount, stat_retry_count = StatRetryCount} =
    State
) ->
  erlang:send_after(?PUT_STAT_TIMEOUT, self(), put_stat),
  request_counter:append({StatReserveCount, StatReleaseCount, StatRetryCount}),
  NewState = State#state{
    stat_release_count = 0,
    stat_reserve_count = 0,
    stat_retry_count = 0
  },
  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

get_requests(Count) -> [get_request() || _ <- lists:seq(1, Count)].

get_request() -> {make_ref(), mock:get_random_request_data()}.
