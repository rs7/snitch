-module(job_server).

-behaviour(gen_server).

%%% api
-export([start_link/5]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

-define(START_MESSAGE, start).

-record(state, {ref, priority, body, controller_ref, list_ref, children}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Ref, Priority, Body, ControllerRef, ListRef) ->
  gen_server:start_link(?SERVER_NAME(Ref), ?MODULE, {Ref, Priority, Body, ControllerRef, ListRef}, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({Ref, Priority, Body, ControllerRef, ListRef}) ->
  self() ! ?START_MESSAGE,
  NewState = #state{ref = Ref, priority = Priority, body = Body, controller_ref = ControllerRef, list_ref = ListRef},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  ?START_MESSAGE,
  #state{
    ref = Ref, priority = Priority, body = {Type, Context}, controller_ref = ControllerRef, list_ref = ListRef
  } = State
) ->
  {ok, ChildBodies} = Type:process(Priority, Context),

  case ChildBodies of
    [] ->
      complete(Ref, ControllerRef, ListRef),
      {noreply, State};

    ChildBodies ->
      ChildRefs = start_children(Ref, Priority, ChildBodies),
      NewChildren = job_children_lib:create(ChildRefs),
      NewState = State#state{children = NewChildren},
      {noreply, NewState}
  end;

handle_info(
  {complete, ChildRef},
  #state{ref = Ref, controller_ref = ControllerRef, list_ref = ListRef, children = Children} = State
) ->
  NewChildren = job_children_lib:remove(ChildRef, Children),

  case job_children_lib:is_empty(NewChildren) of
    true -> complete(Ref, ControllerRef, ListRef);
    false -> ok
  end,

  NewState = State#state{children = NewChildren},
  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

start_children(Ref, Priority, ChildBodies) -> start_children(Ref, Priority, ChildBodies, [], 1).

start_children(_Ref, _Priority, [], ChildRefs, _ChildNumber) -> ChildRefs;

start_children(Ref, Priority, [ChildBody | RemainingChildBodies], ChildRefs, ChildNumber) ->
  ChildPriority = Priority ++ [ChildNumber],
  ChildRef = start_child(Ref, ChildPriority, ChildBody),
  start_children(Ref, Priority, RemainingChildBodies, [ChildRef | ChildRefs], ChildNumber + 1).

start_child(Ref, ChildPriority, ChildBody) ->
  ChildRef = make_ref(),
  {ok, _ChildPid} = job_list:start_job(Ref, ChildRef, ChildPriority, ChildBody, self()),
  ChildRef.

complete(Ref, ControllerRef, ListRef) ->
  send_complete_message(Ref, ControllerRef),
  job:stop(Ref, ListRef).

send_complete_message(Ref, ControllerRef) -> util:send(ControllerRef, {complete, Ref}).
