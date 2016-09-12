-module(reply).

-behaviour(gen_server).

%%% api
-export([start_link/0, call/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Request) -> gen_server:call(?MODULE, Request, infinity).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call(Request, From, State) ->
  spawn_link(
    fun () ->
      timer:sleep(3000),
      gen_server:reply(From, {ok, Request})
    end
  ),
  {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, _State) ->
  io:format("terminate ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
