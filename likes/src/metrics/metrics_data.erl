-module(metrics_data).

-behaviour(gen_server).

%%% api
-export([start_link/0, inc/1, inc/2, get/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIME, erlang:monotonic_time(second)).

-record(state, {data, time}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

inc(Name) -> inc(Name, 1).

inc(Name, Increment) -> gen_server:cast(?MODULE, {inc, Name, Increment}).

get() -> gen_server:call(?MODULE, get).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  State = #state{data = dict:new(), time = ?TIME},
  {ok, State}.

handle_call(get, _From, #state{data = Data, time = Time} = State) ->
  NewTime = ?TIME,
  Duration = NewTime - Time,
  Values = dict:to_list(Data),
  Result = {Duration, Values},
  NewData = dict:new(),
  NewState = State#state{data = NewData, time = NewTime},
  {reply, {ok, Result}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({inc, Name, Increment}, #state{data = Data} = State) ->
  NewData = dict:update_counter(Name, Increment, Data),
  NewState = State#state{data = NewData},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
