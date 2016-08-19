-module(pool_controller).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {worker_count, started}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, WorkerCount} = application:get_env(worker_count),
  self() ! start_worker,
  {ok, #state{worker_count = WorkerCount, started = 0}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start_worker, #state{worker_count = WorkerCount, started = Started} = State) when Started >= WorkerCount ->
  {noreply, State};

handle_info(start_worker, #state{started = Started} = State) ->
  WorkerId = Started + 1,
  start_worker(WorkerId),
  erlang:send_after(1000, self(), start_worker),
  {noreply, State#state{started = Started + 1}};

handle_info({'DOWN', _Reference, process, Pid, Reason}, State) ->
  handle_down_connection(Pid, Reason),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

start_worker(Id) ->
  case pool_supervisor:start_worker(Id) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      lager:info("Worker started ~B ~p", [Id, Pid]),
      Pid;
    {error, _Reason} ->
      lager:error("Worker can't start")
  end.

handle_down_connection(Pid, _Reason) ->
  lager:warning("Worker DOWN ~p", [Pid]).
