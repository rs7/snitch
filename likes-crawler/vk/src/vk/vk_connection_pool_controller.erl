-module(vk_connection_pool_controller).

-behaviour(gen_server).

%% API
-export([start_link/1, start_connection/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection_count}).

%%====================================================================
%% API
%%====================================================================

start_link(ConnectionCount) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [ConnectionCount], []).

%%====================================================================
%% gen_server
%%====================================================================

init([ConnectionCount]) ->
  io:format("ConnectionCount: ~B~n", [ConnectionCount]),
  start_connections(ConnectionCount),
  {ok, #state{connection_count = ConnectionCount}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', _Reference, process, _PID, Reason}, State) ->
  handle_down_connection(Reason),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% internal
%%====================================================================

start_connections(ConnectionCount) -> [start_connection() || _ <- lists:seq(1, ConnectionCount)].

start_connection() ->
  {ok, PID} = vk_connection_pool_supervisor:start_child(),
  erlang:monitor(process, PID),
  PID.

handle_down_connection(Reason) ->
  io:format("Connection DOWN: ~p~n", [Reason]).
