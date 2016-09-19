-module(worker_pool_controller).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_workers_count/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHANGE_WORKERS_COUNT_TIMEOUT, 3000).

-record(state, {workers_count}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_workers_count(Count) -> gen_server:cast(?MODULE, {set_workers_count, Count}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, WorkersCount} = application:get_env(worker_count),
  self() ! update_workers_count,
  NewState = #state{workers_count = WorkersCount},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({set_workers_count, WorkersCount}, State) ->
  self() ! update_workers_count,
  NewState = State#state{workers_count = WorkersCount},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(update_workers_count, #state{workers_count = WorkersCount} = State) ->
  Pids = worker_pool_workers_supervisor:get_workers_pids(),
  CurrentCount = length(Pids),

  if
    WorkersCount =:= CurrentCount -> ok;

    WorkersCount >= CurrentCount ->
      {ok, _Pid} = worker_pool_workers_supervisor:start_child(),
      erlang:send_after(?CHANGE_WORKERS_COUNT_TIMEOUT, self(), update_workers_count);

     WorkersCount =< CurrentCount ->
      ok = worker_pool_workers_supervisor:terminate_child(hd(Pids)),
      erlang:send_after(?CHANGE_WORKERS_COUNT_TIMEOUT, self(), update_workers_count)
  end,

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
