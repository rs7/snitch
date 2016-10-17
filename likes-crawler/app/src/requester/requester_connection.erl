-module(requester_connection).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

-define(SHORT_BLOCK_TIMEOUT, 1000).
-define(REQUEST_COUNT_BY_CONNECTION, 100).

-record(state, {
  requester_ref,
  gun_connection_pid,
  gun_connection_monitor_ref,
  requests_in_progress = #{},
  requests_count_from_up = 0
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> gen_server:start_link(?SERVER_NAME(RequesterRef), ?MODULE, RequesterRef, []).

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
  {gun_up, GunConnectionPid, http}, #state{gun_connection_pid = GunConnectionPid} = State
) ->
  self() ! send_block,
  NewState = State#state{requests_count_from_up = 0},
  {noreply, NewState};

%%%===================================================================
%%% gun_stream_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {RequestRef, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),

  request:error(RequestRef, {gun_stream_error, Reason}),

  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  RequestRef = maps:get(StreamRef, RequestsInProgress),
  request:status(RequestRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {RequestRef, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request:status(RequestRef, Status),
  request:fin(RequestRef),
  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  RequestRef = maps:get(StreamRef, RequestsInProgress),
  request:data(RequestRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress} = State
) ->
  {RequestRef, NewRequestsInProgress} = maps:take(StreamRef, RequestsInProgress),
  request:data(RequestRef, Data),
  request:fin(RequestRef),
  NewState = State#state{requests_in_progress = NewRequestsInProgress},
  {noreply, NewState};

%%%===================================================================
%%% send_block
%%%===================================================================

handle_info(
  send_block,
  #state{
    requester_ref = RequesterRef, gun_connection_pid = GunConnectionPid, requests_in_progress = RequestsInProgress,
    requests_count_from_up = RequestsCountFromUp
  } = State
) ->
  MaxCount = ?REQUEST_COUNT_BY_CONNECTION - RequestsCountFromUp,
  NewRequests = run_requests(RequesterRef, GunConnectionPid, MaxCount),
  NewRequestsInProgress = maps:merge(RequestsInProgress, maps:from_list(NewRequests)),
  SuccessCount = length(NewRequests),
  NewRequestsCountFromUp = RequestsCountFromUp + SuccessCount,

  lager:debug("send_block ~B/~B", [SuccessCount, MaxCount]),

  case SuccessCount of
    MaxCount -> ok;
    SuccessCount -> erlang:send_after(?SHORT_BLOCK_TIMEOUT, self(), send_block)
  end,

  NewState = State#state{requests_in_progress = NewRequestsInProgress, requests_count_from_up = NewRequestsCountFromUp},
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

run_requests(RequesterRef, GunConnectionPid, Count) ->
  {ok, RequestInfos} = requester_controller:reserve(RequesterRef, Count),
  run_many(GunConnectionPid, RequesterRef, RequestInfos).

run_many(_GunConnectionPid, _RequesterRef, []) -> [];

run_many(GunConnectionPid, RequesterRef, [RequestInfo | RemainingRequestInfos]) -> [
  run_one(GunConnectionPid, RequesterRef, RequestInfo)
  |
  run_many(GunConnectionPid, RequesterRef, RemainingRequestInfos)
].

run_one(GunConnectionPid, RequesterRef, {RequestRef, RequestData}) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  run_request_server(RequesterRef, RequestRef, StreamRef).

run_request_server(RequesterRef, RequestRef, StreamRef) ->
  request:start_link(RequestRef, RequesterRef),
  {StreamRef, RequestRef}.

%%%===================================================================
%%% requests_in_progress_error
%%%===================================================================

requests_in_progress_error(RequestsInProgress, Reason) -> [
  request:error(RequestRef, Reason) || RequestRef <- maps:values(RequestsInProgress)
].
