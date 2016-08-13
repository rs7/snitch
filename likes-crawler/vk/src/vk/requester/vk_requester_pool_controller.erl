-module(vk_requester_pool_controller).

-behaviour(gen_server).

%% api
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {requesters_count}).

%%====================================================================
%% api
%%====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server
%%====================================================================

init([]) ->
  self() ! start_requesters,
  {ok, #state{requesters_count = 300}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start_requesters, #state{requesters_count = RequestersCount} = State) ->
  start_requesters(RequestersCount),
  {noreply, State};

handle_info({'DOWN', _Reference, process, Pid, Reason}, State) ->
  handle_down_connection(Pid, Reason),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% internal
%%====================================================================

start_requesters(Count) -> [start_requester(N * 1000) || N <- lists:seq(1, Count)], ok.

start_requester(Sleep) ->
  case vk_requester_pool_supervisor:start_child(Sleep) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      Pid;
    {error, _Reason} ->
      lager:error("Requester can't start")
  end.

handle_down_connection(Pid, _Reason) ->
  lager:warning("Requester DOWN ~p", [Pid]).
