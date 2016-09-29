-module(requester_pool_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, get_requester_count/0, set_requester_count/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHANGE_REQUESTER_COUNT_TIMEOUT, 2000).

-record(state, {set_count = 0, current_count = 0, requester_ref_items = []}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterCount) -> gen_server:start_link({local, ?MODULE}, ?MODULE, RequesterCount, []).

get_requester_count() -> gen_server:call(?MODULE, get_requester_count).

set_requester_count(RequesterCount) -> gen_server:cast(?MODULE, {set_requester_count, RequesterCount}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterCount) ->
  folsom_metrics:new_counter(pool),
  prometheus_counter:new([{name, pool}, {help, "Pool size"}]),
  self() ! update_count,
  NewState = #state{set_count = RequesterCount},
  {ok, NewState}.

handle_call(get_requester_count, _From, #state{set_count = SetCount, current_count = CurrentCount} = State) ->
  Result = {CurrentCount, SetCount},
  {reply, {ok, Result}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({set_requester_count, RequesterCount}, State) ->
  self() ! update_count,
  NewState = State#state{set_count = RequesterCount},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(update_count, #state{set_count = SetCount, current_count = SetCount} = State) -> {noreply, State};

handle_info(
  update_count,
  #state{set_count = SetCount, current_count = CurrentCount, requester_ref_items = RequesterRefItems} = State
) when CurrentCount < SetCount ->
  RequesterRef = make_ref(),
  {ok, _RequesterPid} = requester_pool_children:start_child(RequesterRef),
  erlang:send_after(?CHANGE_REQUESTER_COUNT_TIMEOUT, self(), update_count),
  folsom_metrics:notify({pool, {inc, 1}}),
  prometheus_counter:inc(pool),
  NewState = State#state{current_count = CurrentCount + 1, requester_ref_items = [RequesterRef | RequesterRefItems]},
  {noreply, NewState};

handle_info(
  update_count,
  #state{
    set_count = TargetCount, current_count = CurrentCount,
    requester_ref_items = [RequesterRef | RemainingRequesterRefItems]
  } = State
) when CurrentCount > TargetCount ->
  ok = requester_pool_children:terminate_child(RequesterRef),
  erlang:send_after(?CHANGE_REQUESTER_COUNT_TIMEOUT, self(), update_workers_count),
  folsom_metrics:notify({pool, {dec, 1}}),
  NewState = State#state{current_count = CurrentCount - 1, requester_ref_items = RemainingRequesterRefItems},
  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
