-module(process_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, get_targeted/0, set_targeted/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  heap = [],
  targeted,
  reserved = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_targeted() -> gen_server:call(?MODULE, get_targeted).

set_targeted(Targeted) -> gen_server:call(?MODULE, {set_targeted, Targeted}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(TargetedList) ->
  NewTargeted = ordsets:from_list(TargetedList),
  NewState = #state{targeted = NewTargeted},
  {ok, NewState}.

handle_call(get_targeted, _From, #state{targeted = Targeted} = State) -> {reply, {ok, Targeted}, State};

handle_call({set_targeted, TargetedList}, _From, State) ->
  NewTargeted = ordsets:from_list(TargetedList),
  NewState = State#state{targeted = NewTargeted},
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
