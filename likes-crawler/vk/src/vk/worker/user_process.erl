-module(user_process).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {worker_id}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> gen_server:start_link(?MODULE, WorkerId, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  gproc:reg({n, l, {?MODULE, WorkerId}}),
  self() ! loop,
  {ok, #state{worker_id = WorkerId}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(loop, #state{worker_id = WorkerId} = State) ->
  {_Pid, Requester} = gproc:await({n, l, {requester, WorkerId}}),
  _Result = hidden_friends:process(users:get(), call_function(Requester)),
  erlang:send_after(0, self(), loop),
  {noreply, State#state{}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

call_function(Requester) -> fun(Request) -> requester:call(Requester, Request) end.
