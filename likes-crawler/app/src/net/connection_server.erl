-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_coworkers/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(REQUEST_COUNT_BY_CONNECTION, 100).

-record(state, {
  requester_pid,
  gun_connection_pid,
  gun_connection_monitor_ref,
  requests_in_progress = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_coworkers(ConnectionPid, Coworkers) -> gen_server:call(ConnectionPid, {set_coworkers, Coworkers}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

%%%===================================================================
%%% set_coworkers
%%%===================================================================

handle_call(
  {set_coworkers, [RequesterPid]}, _From,
  #state{requester_pid = undefined} = State
) ->
  self() ! connect,

  NewState = State#state{requester_pid = RequesterPid},
  {reply, ok, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

%%%===================================================================
%%% 'DOWN'
%%%===================================================================

handle_info(
  {'DOWN', GunConnectionMonitorRef, process, GunConnectionPid, Reason},
  #state{
    gun_connection_pid = GunConnectionPid, gun_connection_monitor_ref = GunConnectionMonitorRef,
    requests_in_progress = RequestsInProgress
  } = State
) ->
  requests_in_progress_error(RequestsInProgress, {gun_DOWN, Reason}),
  NewState = State#state{
    gun_connection_pid = undefined, gun_connection_monitor_ref = undefined, requests_in_progress = #{}
  },
  {stop, {'DOWN', Reason}, NewState};

%%%===================================================================
%%% gun_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  requests_in_progress_error(RequestsInProgress, {gun_error, Reason}),
  NewState = State#state{requests_in_progress = #{}},
  {stop, {gun_error, Reason}, NewState};

%%%===================================================================
%%% gun_down
%%%===================================================================

handle_info(
  {gun_down, GunConnectionPid, http, Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  if
    map_size(RequestsInProgress) =/= 0 ->
      lager:warning("unexpected gun_down ~p", [Reason]),
      requests_in_progress_error(RequestsInProgress, {gun_down, Reason});
    true -> ok
  end,
  NewState = State#state{requests_in_progress = #{}},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid, requester_pid = RequesterPid} = State
) ->
  RequestsInProgress = maps:from_list(run_block(GunConnectionPid, RequesterPid)),
  NewState = State#state{requests_in_progress = RequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% gun_stream_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {{RequestPid, RequestRef}, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),

  request_server:error(RequestPid, RequestRef, {gun_stream_error, Reason}),

  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {RequestPid, RequestRef} = maps:get(StreamRef, RequestsInProgress),
  request_server:status(RequestPid, RequestRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {{RequestPid, RequestRef}, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request_server:status(RequestPid, RequestRef, Status),
  request_server:fin(RequestPid, RequestRef),
  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {RequestPid, RequestRef} = maps:get(StreamRef, RequestsInProgress),
  request_server:data(RequestPid, RequestRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {{RequestPid, RequestRef}, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request_server:data(RequestPid, RequestRef, Data),
  request_server:fin(RequestPid, RequestRef),
  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% connect
%%%===================================================================

handle_info(connect, State) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  NewState = State#state{
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef
  },

  {noreply, NewState};

%%%===================================================================

handle_info(UnexpectedMessage, State) ->
  lager:warning("unexpected message ~p", [UnexpectedMessage]),
  {noreply, State}.

terminate(
  Reason,
  #state{
    gun_connection_pid = GunConnectionPid, gun_connection_monitor_ref = GunConnectionMonitorRef,
    requests_in_progress = RequestsInProgress
  }
) ->
  case GunConnectionPid of
    undefined -> ok;
    _ -> gun:close(GunConnectionPid)
  end,

  case GunConnectionMonitorRef of
    undefined -> ok;
    _ -> demonitor(GunConnectionMonitorRef)
  end,

  requests_in_progress_error(RequestsInProgress, {terminate, Reason}),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

run_block(GunConnectionPid, RequesterPid) ->
  {ok, RequestInfos} = requester_server:reserve(RequesterPid, ?REQUEST_COUNT_BY_CONNECTION),

  Count = length(RequestInfos),
  if
    Count < ?REQUEST_COUNT_BY_CONNECTION -> lager:warning("short request block (size: ~B)", [Count]);
    true -> ok
  end,

  run_requests(GunConnectionPid, RequesterPid, RequestInfos).

run_requests(_GunConnectionPid, _RequesterPid, []) -> [];

run_requests(GunConnectionPid, RequesterPid, [RequestInfo]) ->
  [run_one(GunConnectionPid, RequesterPid, RequestInfo, close)];

run_requests(GunConnectionPid, RequesterPid, [RequestInfo | RemainingRequestInfos]) -> [
  run_one(GunConnectionPid, RequesterPid, RequestInfo)
  |
  run_requests(GunConnectionPid, RequesterPid, RemainingRequestInfos)
].

run_one(GunConnectionPid, RequesterPid, {RequestRef, RequestData}, close) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData, close),
  run_request_server(RequesterPid, RequestRef, StreamRef).

run_one(GunConnectionPid, RequesterPid, {RequestRef, RequestData}) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  run_request_server(RequesterPid, RequestRef, StreamRef).

run_request_server(RequesterPid, RequestRef, StreamRef) ->
  {ok, RequestPid} = request_server:start_link(RequesterPid, RequestRef),
  {StreamRef, {RequestPid, RequestRef}}.

%%%===================================================================
%%% requests_in_progress_error
%%%===================================================================

requests_in_progress_error(RequestsInProgress, _Reason) when map_size(RequestsInProgress) =:= 0 -> ok;

requests_in_progress_error(RequestsInProgress, Reason) -> [
  request_server:error(RequestPid, RequestRef, Reason) || {RequestPid, RequestRef} <- maps:values(RequestsInProgress)
].
