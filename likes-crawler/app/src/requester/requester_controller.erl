-module(requester_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../util/identified_name.hrl").

-define(SERVER_NAME(RequesterRef), ?IDENTIFIED_NAME(?MODULE, RequesterRef)).

-record(state, {reserve = #{}}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> gen_server:start_link(?SERVER_NAME(RequesterRef), ?MODULE, [], []).

reserve(RequesterRef, RequestCount) -> gen_server:call(?SERVER_NAME(RequesterRef), {reserve, RequestCount}, infinity).

release(RequesterRef, RequestRef, Result) -> gen_server:cast(?SERVER_NAME(RequesterRef), {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  NewReserve = #{},
  NewState = #state{reserve = NewReserve},
  {ok, NewState}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call({reserve, Count}, _From, #state{reserve = Reserve} = State) ->

  {ok, Items} = call_queue:take(Count),

  Result = [{RequestRef, RequestData} || {_Priority, {RequestRef, RequestData, _RequestFrom}} <- Items],

  NewReserve = maps:merge(
    Reserve,
    maps:from_list(
      [{RequestRef, {Priority, RequestData, From}} || {Priority, {RequestRef, RequestData, From}} <- Items]
    )
  ),

  NewState = State#state{reserve = NewReserve},
  {reply, {ok, Result}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({release, RequestRef, {result, Result}}, #state{reserve = Reserve} = State) ->
  {Item, NewReserve} = maps:take(RequestRef, Reserve),

  {_Priority, _RequestData, RequestFrom} = Item,

  gen_server:reply(RequestFrom, {ok, Result}),

  NewState = State#state{reserve = NewReserve},
  {noreply, NewState};

handle_cast({release, RequestRef, {retry, Reason}}, #state{reserve = Reserve} = State) ->
  {Item, NewReserve} = maps:take(RequestRef, Reserve),

  lager:warning("retry ~p ~p", [Reason, Item]),

  {Priority, RequestData, From} = Item,

  call_queue:add(Priority, RequestData, From),

  NewState = State#state{reserve = NewReserve},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
