-module(worker_pool_controller).

-behaviour(gen_server).

%%% api
-export([start_link/0, get_workers_count/0, set_workers_count/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHANGE_WORKERS_COUNT_TIMEOUT, 3000).

-record(state, {target_count = 0, current_count = 0}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, WorkersCount} = application:get_env(workers_count),
  gen_server:start_link({local, ?MODULE}, ?MODULE, WorkersCount, []).

get_workers_count() -> gen_server:call(?MODULE, get_workers_count).

set_workers_count(Count) -> gen_server:cast(?MODULE, {set_workers_count, Count}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkersCount) ->
  self() ! update_workers_count,
  NewState = #state{target_count = WorkersCount},
  {ok, NewState}.

handle_call(get_workers_count, _From, #state{target_count = TargetCount, current_count = CurrentCount} = State) ->
  Result = {CurrentCount, TargetCount},
  {reply, {ok, Result}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({set_workers_count, WorkersCount}, State) ->
  self() ! update_workers_count,
  NewState = State#state{target_count = WorkersCount},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(update_workers_count, #state{target_count = Count, current_count = Count} = State) -> {noreply, State};

handle_info(
  update_workers_count, #state{target_count = TargetCount, current_count = CurrentCount} = State
) when CurrentCount < TargetCount ->
  WorkerId = CurrentCount + 1,
  {ok, _Pid} = worker_pool_workers_supervisor:start_child(WorkerId),
  erlang:send_after(?CHANGE_WORKERS_COUNT_TIMEOUT, self(), update_workers_count),
  NewState = State#state{current_count = CurrentCount + 1},
  {noreply, NewState};

handle_info(
  update_workers_count, #state{target_count = TargetCount, current_count = CurrentCount} = State
) when CurrentCount > TargetCount ->
  WorkerId = CurrentCount,
  ok = worker_pool_workers_supervisor:terminate_child(WorkerId),
  erlang:send_after(?CHANGE_WORKERS_COUNT_TIMEOUT, self(), update_workers_count),
  NewState = State#state{current_count = CurrentCount - 1},
  {noreply, NewState};



handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
