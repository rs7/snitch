-module(connection).

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
  streams,
  requests_count_from_up
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
  link(GunConnectionPid),
  NewState = #state{requester_ref = RequesterRef, gun_connection_pid = GunConnectionPid},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

%%%===================================================================
%%% gun_down
%%%===================================================================

handle_info(
  {gun_down, GunConnectionPid, http, Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  streams_error(Streams, {gun_down, Reason}),
  NewState = State#state{streams = undefined},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http}, #state{gun_connection_pid = GunConnectionPid} = State
) ->
  self() ! send_block,
  NewState = State#state{requests_count_from_up = 0, streams = sets:new()},
  {noreply, NewState};

%%%===================================================================
%%% gun_stream_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  stream:error(StreamRef, {gun_stream_error, Reason}),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  stream:status(StreamRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  stream:status(StreamRef, Status),
  stream:fin(StreamRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  stream:data(StreamRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  stream:data(StreamRef, Data),
  stream:fin(StreamRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% send_block
%%%===================================================================

handle_info(
  send_block,
  #state{
    requester_ref = RequesterRef, gun_connection_pid = GunConnectionPid, streams = RequestsInProgress,
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

  NewState = State#state{streams = NewRequestsInProgress, requests_count_from_up = NewRequestsCountFromUp},
  {noreply, NewState};

%%%===================================================================

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{gun_connection_pid = GunConnectionPid, streams = Streams}) ->
  case GunConnectionPid of
    undefined -> ok;
    _ -> gun:close(GunConnectionPid)
  end,

  streams_error(Streams, {terminate, Reason}),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

run_requests(RequesterRef, GunConnectionPid, Count) ->
  {ok, RequestInfos} = requester_controller:get(RequesterRef, Count),
  run_many(GunConnectionPid, RequesterRef, RequestInfos, []).

run_many(_GunConnectionPid, _RequesterRef, [], Acc) -> Acc;

run_many(GunConnectionPid, RequesterRef, [RequestInfo | RemainingRequestInfos], Acc) ->
  Result = run_one(GunConnectionPid, RequesterRef, RequestInfo),
  run_many(GunConnectionPid, RequesterRef, RemainingRequestInfos, [Result | Acc]).

run_one(GunConnectionPid, RequesterRef, {RequestRef, RequestData}) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  stream:start_link(RequestRef, RequesterRef),
  {StreamRef, RequestRef}.

%%%===================================================================
%%% requests_in_progress_error
%%%===================================================================

streams_error(RequestsInProgress, Reason) -> [
  stream:error(RequestRef, Reason) || RequestRef <- maps:values(RequestsInProgress)
].
