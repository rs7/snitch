-module(stat).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, 60000).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  self() ! timeout,
  {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(timeout, State) ->
  erlang:send_after(?TIMEOUT, self(), timeout),
  lager:info(
    %"erlang ~p "
    "global ~p "
    "local ~p "
    "pool ~p "
    , [
      %metrics:metrics(),
      gen_heap:get_size({global, root_heap}),
      gen_heap:get_size(local_heap),
      worker_pool:get_workers_count()
    ]),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
