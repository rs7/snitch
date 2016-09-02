-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STAT_TIMEOUT, 10 * 1000).

-record(state, {
  reserve_count,
  release_count,
  retry_count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

reserve(RequesterPid, Count) -> gen_server:call(RequesterPid, {reserve, Count}).

release(RequesterPid, RequestRef, Result) -> gen_server:cast(RequesterPid, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  erlang:send_after(?STAT_TIMEOUT, self(), save_stat),

  {ok, #state{
    reserve_count = 0,
    release_count = 0,
    retry_count = 0
  }}.

handle_call({reserve, Count}, _From, #state{reserve_count = ReserveCount} = State) ->
  Reply = [create_request() || _ <- lists:seq(1, Count)],

  {reply, {ok, Reply}, State#state{reserve_count = ReserveCount + Count}};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({release, RequestRef, {result, Result}}, #state{release_count = ReleaseCount} = State) ->
  {noreply, State#state{release_count = ReleaseCount + 1}};

handle_cast(
  {release, RequestRef, {retry, Release}},
  #state{release_count = ReleaseCount, retry_count = RetryCount} = State
) ->
  lager:warning("retry ~p", [Release]),
  {noreply, State#state{release_count = ReleaseCount + 1, retry_count = RetryCount + 1}};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  save_stat,
  #state{reserve_count = ReserveCount, release_count = ReleaseCount, retry_count = RetryCount} = State
) ->
  erlang:send_after(?STAT_TIMEOUT, self(), save_stat),
  request_counter:append({ReserveCount, ReleaseCount, RetryCount}),
  {noreply, State#state{release_count = 0, reserve_count = 0, retry_count = 0}};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

create_request() -> {make_ref(), mock:get_request_data()}.
