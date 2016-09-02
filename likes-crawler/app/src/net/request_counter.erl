-module(request_counter).

-behaviour(gen_server).

%%% api
-export([start_link/0, append/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOG_TIMEOUT, 60 * 1000).

-record(state, {
  reserve_count,
  release_count,
  retry_count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

append(Counts) -> gen_server:cast(?MODULE, {append, Counts}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  self() ! log,
  {ok, #state{
    reserve_count = 0,
    release_count = 0,
    retry_count = 0
  }}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(
  {append, {AddReserveCount, AddReleaseCount, AddRetryCount}},
  #state{reserve_count = ReserveCount, release_count = ReleaseCount, retry_count = RetryCount} = State
) ->
  {noreply, State#state{
    reserve_count = ReserveCount + AddReserveCount,
    release_count = ReleaseCount + AddReleaseCount,
    retry_count = RetryCount + AddRetryCount
  }};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  log,
  #state{reserve_count = ReserveCount, release_count = ReleaseCount, retry_count = RetryCount} = State
) ->
  lager:info("reserve ~B release ~B retry ~B", [ReserveCount, ReleaseCount, RetryCount]),
  erlang:send_after(?LOG_TIMEOUT, self(), log),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
