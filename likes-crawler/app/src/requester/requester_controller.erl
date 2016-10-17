-module(requester_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, reserve/2, release/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

-record(state, {}).

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
  NewState = #state{},
  {ok, NewState}.

%%%===================================================================
%%% reserve
%%%===================================================================

handle_call({reserve, Count}, _From, #state{} = State) ->
  Result = [],

  NewState = State#state{},
  {reply, {ok, Result}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% release
%%%===================================================================

handle_cast({release, RequestRef, {result, Result}}, #state{} = State) ->
  NewState = State#state{},
  {noreply, NewState};

handle_cast({release, RequestRef, {retry, Reason}}, #state{} = State) ->
  lager:warning("retry ~p", [Reason]),
  NewState = State#state{},
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
