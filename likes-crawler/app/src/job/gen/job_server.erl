-module(job_server).

-behaviour(gen_server).

%%% api
-export([start_link/4]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../util/identified_name.hrl").

-define(SERVER_NAME(JobRef), ?IDENTIFIED_NAME(?MODULE, JobRef)).

-define(START_MESSAGE, start).

-record(state, {ref, priority, body, controller_ref, children}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Ref, Priority, Body, ControllerRef) ->
  gen_server:start_link(?SERVER_NAME(Ref), ?MODULE, {Ref, Priority, Body, ControllerRef}, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({Ref, Priority, Body, ControllerRef}) ->
  self() ! ?START_MESSAGE,
  NewState = #state{ref = Ref, priority = Priority, body = Body, controller_ref = ControllerRef},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  ?START_MESSAGE,
  #state{
    ref = Ref, priority = Priority, body = {Type, Context}, controller_ref = ControllerRef
  } = State
) ->
  {ok, ChildBodies} = Type:process(Priority, Context),

  case ChildBodies of
    [] ->
      complete(Ref, ControllerRef),
      {noreply, State};

    ChildBodies ->
      ChildRefs = start_children(Ref, Priority, ChildBodies),
      NewChildren = job_children_lib:create(ChildRefs),
      NewState = State#state{children = NewChildren},
      {noreply, NewState}
  end;

handle_info(
  {complete, ChildRef}, #state{ref = Ref, controller_ref = ControllerRef, children = Children} = State
) ->
  NewChildren = job_children_lib:remove(ChildRef, Children),

  case job_children_lib:is_empty(NewChildren) of
    true -> complete(Ref, ControllerRef);
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
  ControllerRef = self(), %?SERVER_NAME(Ref)
  ChildArgs = [ChildRef, ChildPriority, ChildBody, ControllerRef],
  {ok, _ChildPid} = job_list:start_job(Ref, ChildArgs),
  ChildRef.

complete(Ref, ControllerRef) ->
  send_complete_message(Ref, ControllerRef),
  job:stop(Ref).

send_complete_message(Ref, ControllerRef) -> util:send(ControllerRef, {complete, Ref}).
