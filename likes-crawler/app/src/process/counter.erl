-module(counter).

-behaviour(gen_server).

%%% api
-export([start_link/0, get/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {first, last, increment, current}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() -> gen_server:call(?MODULE, get).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  First = 1,
  Last = 380000000,
  Count = 10000,
  {ok, #state{
    first = First,
    last = Last,
    increment = util:ceil((Last - First) / Count),
    current = First
  }}.

handle_call(get, _From, #state{current = Current, increment = Increment, last = Last} = State)
  when Current + Increment =< Last ->
  {reply, {user, Current}, State#state{current = Current + Increment}};

handle_call(get, _From, #state{current = Current, increment = Increment, last = Last, first = First} = State)
  when Current + Increment > Last ->
  {reply, undefined, State#state{current = First}};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
