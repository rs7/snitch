-module(worker_process).

-behaviour(gen_server).

%%% api
-export([start_link/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {worker_id, process}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId, Process) -> gen_server:start_link(?MODULE, {WorkerId, Process}, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({WorkerId, Process}) ->
  gproc:reg({n, l, {?MODULE, WorkerId}}),
  self() ! loop,
  {ok, #state{worker_id = WorkerId, process = Process}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(loop, #state{worker_id = WorkerId, process = Process} = State) ->
  case Process(call_function(WorkerId)) of
    next ->
      self() ! loop,
      {noreply, State};
    stop ->
      {noreply, State, hibernate}
  end.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

call_function(WorkerId) ->
  fun(Request) ->
    call(WorkerId, Request)
  end.

call(WorkerId, Request) ->
  {_Pid, Requester} = gproc:await({n, l, {worker_requester, WorkerId}}),
  worker_requester:call(Requester, Request).
