-module(requester_connection).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../util/identified_name.hrl").

-define(REQUEST_COUNT_BY_CONNECTION, 100).

-record(state, {
  requester_ref,
  gun_connection_pid,
  gun_connection_monitor_ref,
  requests_in_progress = #{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> gen_server:start_link(?IDENTIFIED_NAME(?MODULE, RequesterRef), ?MODULE, RequesterRef, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterRef) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  NewState = #state{
    requester_ref = RequesterRef,
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef
  },
  {ok, NewState}.

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
  requests_in_progress_error(RequestsInProgress, {gun_connection_down, Reason}),
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
) when map_size(RequestsInProgress) =/= 0 ->
  requests_in_progress_error(RequestsInProgress, {gun_down, Reason}),
  NewState = State#state{requests_in_progress = #{}},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid, requester_ref = RequesterRef} = State
) ->
  RequestsInProgress = maps:from_list(run_block(GunConnectionPid, RequesterRef, ?REQUEST_COUNT_BY_CONNECTION)),
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

  request:error(RequestPid, RequestRef, {gun_stream_error, Reason}),

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
  request:status(RequestPid, RequestRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {{RequestPid, RequestRef}, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request:status(RequestPid, RequestRef, Status),
  request:fin(RequestPid, RequestRef),
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
  request:data(RequestPid, RequestRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {{RequestPid, RequestRef}, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request:data(RequestPid, RequestRef, Data),
  request:fin(RequestPid, RequestRef),
  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================

handle_info(_Info, State) -> {noreply, State}.

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

run_block(GunConnectionPid, RequesterRef, Count) ->
  {ok, RequestInfos} = requester_controller:reserve(RequesterRef, Count),

  run_requests(GunConnectionPid, RequesterRef, RequestInfos).

run_requests(_GunConnectionPid, _RequesterRef, []) -> [];

run_requests(GunConnectionPid, RequesterRef, [RequestInfo]) ->
  [run_one(GunConnectionPid, RequesterRef, RequestInfo, close)];

run_requests(GunConnectionPid, RequesterRef, [RequestInfo | RemainingRequestInfos]) -> [
  run_one(GunConnectionPid, RequesterRef, RequestInfo)
  |
  run_requests(GunConnectionPid, RequesterRef, RemainingRequestInfos)
].

run_one(GunConnectionPid, RequesterRef, {RequestRef, RequestData}, close) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData, close),
  run_request_server(RequesterRef, RequestRef, StreamRef).

run_one(GunConnectionPid, RequesterRef, {RequestRef, RequestData}) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  run_request_server(RequesterRef, RequestRef, StreamRef).

run_request_server(RequesterRef, RequestRef, StreamRef) ->
  {ok, RequestPid} = request:start_link(RequesterRef, RequestRef),
  {StreamRef, {RequestPid, RequestRef}}.

%%%===================================================================
%%% requests_in_progress_error
%%%===================================================================

requests_in_progress_error(RequestsInProgress, _Reason) when map_size(RequestsInProgress) =:= 0 -> ok;

requests_in_progress_error(RequestsInProgress, Reason) -> [
  request:error(RequestPid, RequestRef, Reason) || {RequestPid, RequestRef} <- maps:values(RequestsInProgress)
].
