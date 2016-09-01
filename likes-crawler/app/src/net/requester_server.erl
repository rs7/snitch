-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, result/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  connection_pid
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> gen_server:start_link(?MODULE, WorkerId, []).

result(RequesterPid, Result) -> gen_server:cast(RequesterPid, {result, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  process_flag(trap_exit, true),

  {ConnectionPid, _} = gproc:await(connection_server:gproc_key(WorkerId)),

  self() ! start,

  {ok, #state{
    connection_pid = ConnectionPid
  }}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({result, Result}, State) ->
  lager:debug("result ~p", [Result]),

  {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start, #state{connection_pid = ConnectionPid} = State) ->
  RequestData = mock:get_request_data(),
  request_server:start_link(self(), ConnectionPid, RequestData),

  lager:debug("request ~p", [RequestData]),

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
