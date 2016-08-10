-module(vk_requester_pool_controller).

-behaviour(gen_server).

%% api
-export([start_link/1, start_requester/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% api
%%====================================================================

start_link(RequestersCount) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [RequestersCount], []).

%%====================================================================
%% gen_server
%%====================================================================

init([RequestersCount]) ->
  io:format("Requester count: ~B~n", [RequestersCount]),
  start_requesters(RequestersCount),
  {ok, #state{}}.

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

start_requesters(Count) -> [start_requester() || _ <- lists:seq(1, Count)].

start_requester() ->
  {ok, PID} = vk_requester_pool_supervisor:start_child(),
  erlang:monitor(process, PID),
  PID.

handle_down_connection(Reason) ->
  io:format("Requester DOWN: ~p~n", [Reason]).
