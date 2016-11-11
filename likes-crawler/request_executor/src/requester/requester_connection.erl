-module(requester_connection).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterId), {via, identifiable, {?MODULE, RequesterId}}).

-define(SHORT_BLOCK_TIMEOUT, 1000).
-define(COUNT_BY_CONNECTION, 100).

-record(state, {
  requester_id,
  gun_connection_pid,
  streams,
  count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterId) -> gen_server:start_link(?SERVER_NAME(RequesterId), ?MODULE, RequesterId, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(RequesterId) ->
  {ok, GunConnectionPid} = connection_lib:open(),
  link(GunConnectionPid),
  NewState = #state{requester_id = RequesterId, gun_connection_pid = GunConnectionPid},
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
  NewState = State#state{count = ?COUNT_BY_CONNECTION, streams = sets:new()},
  {noreply, NewState};

%%%===================================================================
%%% gun_stream_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  requester_stream:error(StreamRef, {gun_stream_error, Reason}),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  requester_stream:status(StreamRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  requester_stream:status(StreamRef, Status),
  requester_stream:fin(StreamRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  requester_stream:data(StreamRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = sets:del_element(StreamRef, Streams),
  requester_stream:data(StreamRef, Data),
  requester_stream:fin(StreamRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% send_block
%%%===================================================================

handle_info(
  send_block,
  #state{
    requester_id = RequesterId, gun_connection_pid = GunConnectionPid, streams = Streams, count = Count
  } = State
) ->

  NewRequests = run_requests(RequesterId, GunConnectionPid, Count),
  NewStreams = sets:union(Streams, sets:from_list(NewRequests)),
  SuccessCount = length(NewRequests),
  NewCount = Count - SuccessCount,

  case NewCount of
    0 -> ok;
    _ -> erlang:send_after(?SHORT_BLOCK_TIMEOUT, self(), send_block)
  end,

  NewState = State#state{streams = NewStreams, count = NewCount},
  {noreply, NewState};

%%%===================================================================

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{gun_connection_pid = GunConnectionPid}) ->
  gun:close(GunConnectionPid),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

run_requests(RequesterId, GunConnectionPid, Count) ->
  {ok, RequestInfoItems} = requester_queue:get(RequesterId, Count),
  [
    begin
      StreamRef = connection_lib:request(GunConnectionPid, RequestData),
      requester_stream:start_link(StreamRef, RequesterId, RequestId),
      StreamRef
    end
    ||
    {RequestId, RequestData} <- RequestInfoItems
  ].

%%%===================================================================
%%% requests_in_progress_error
%%%===================================================================

streams_error(RequestsInProgress, Reason) -> [
  requester_stream:error(RequestRef, Reason) || RequestRef <- maps:values(RequestsInProgress)
].
