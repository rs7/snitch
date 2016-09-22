-module(call_queue).

-behaviour(gen_server).

%%% api
-export([start_link/0, call/2, take/1, add/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME, {global, ?MODULE}).

-record(state, {data}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?SERVER_NAME, ?MODULE, [], []).

call(Priority, RequestData) -> gen_server:call(?SERVER_NAME, {call, Priority, RequestData}, infinity).

add(Priority, RequestData, From) -> gen_server:cast(?SERVER_NAME, {add, Priority, RequestData, From}).

take(Count) -> gen_server:call(?SERVER_NAME, {take, Count}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  NewData = call_queue_data:new(),
  NewState = #state{data = NewData},
  {ok, NewState}.

handle_call({call, Priority, RequestData}, From, State) -> handle_cast({add, Priority, RequestData, From}, State);

handle_call({take, Count}, _From, #state{data = Data} = State) ->
  {Items, NewData} = call_queue_data:take(Count, Data),
  NewState = State#state{data = NewData},
  {reply, {ok, Items}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({add, Priority, RequestData, From}, #state{data = Data} = State) ->
  RequestRef = make_ref(),
  Item = {RequestRef, RequestData, From},
  NewData = call_queue_data:add(Priority, Item, Data),
  NewState = State#state{data = NewData},
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
