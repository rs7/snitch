-module(test).

-behaviour(gen_server).

%%% api
-export([start_link/1, which_search/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(local) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
start_link(via) -> gen_server:start_link({via, gproc, {n, l, ?MODULE}}, ?MODULE, [], []);
start_link(global) -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

which_search() -> 1052662.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(Info, State) ->
  lager:info("(!) test info - ~p ~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
