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
  streams
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_coworkers(ConnectionPid, Coworkers) -> gen_server:call(ConnectionPid, {set_coworkers, Coworkers}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, #state{
    streams = #{}
  }}.

%%%===================================================================
%%% set_coworkers
%%%===================================================================

handle_call(
  {set_coworkers, [RequesterPid]}, _From,
  #state{requester_pid = undefined} = State
) ->
  self() ! connect,

  NewState = State#state{
    requester_pid = RequesterPid
  },

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
    gun_connection_pid = GunConnectionPid, gun_connection_monitor_ref = GunConnectionMonitorRef, streams = Streams
  } = State
) ->
  requests_error(Streams, {'DOWN', Reason}),
  NewState = State#state{streams = #{}},
  {stop, {'DOWN', Reason}, NewState};

%%%===================================================================
%%% gun_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  requests_error(Streams, {gun_error, Reason}),
  NewState = State#state{streams = #{}},
  {stop, {gun_error, Reason}, NewState};

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, RequestRef}, NewStreams} = maps:take(StreamRef, Streams),

  request_server:error(RequestPid, RequestRef, {gun_stream_error, Reason}),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid, requester_pid = RequesterPid} = State
) ->
  Streams = maps:from_list(send(GunConnectionPid, RequesterPid)),
  NewState = State#state{
    streams = Streams
  },
  {noreply, NewState};

%%%===================================================================
%%% gun_down
%%%===================================================================

handle_info(
  {gun_down, GunConnectionPid, http, Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  requests_error(Streams, {gun_down, Reason}),
  NewState = State#state{streams = #{}},
  {noreply, NewState};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, RequestRef} = maps:get(StreamRef, Streams),
  request_server:status(RequestPid, RequestRef, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, RequestRef}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:status(RequestPid, RequestRef, Status),
  request_server:fin(RequestPid, RequestRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, RequestRef} = maps:get(StreamRef, Streams),
  request_server:data(RequestPid, RequestRef, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, RequestRef}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:data(RequestPid, RequestRef, Data),
  request_server:fin(RequestPid, RequestRef),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================

handle_info(connect, State) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  NewState = State#state{
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef
  },

  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

send(GunConnectionPid, RequesterPid) ->
  {ok, RequestInfos} = requester_server:reserve(RequesterPid, ?REQUEST_COUNT_BY_CONNECTION),

  Count = length(RequestInfos),

  if
    Count < ?REQUEST_COUNT_BY_CONNECTION -> lager:warning("short request block (size: ~B)", [Count]);
    true -> ok
  end,

  send(GunConnectionPid, RequesterPid, RequestInfos).

send(GunConnectionPid, RequesterPid, [RequestInfo]) -> [send_one(GunConnectionPid, RequesterPid, RequestInfo, close)];

send(GunConnectionPid, RequesterPid, [RequestInfo | RemainingRequestInfos]) ->
  [
    send_one(GunConnectionPid, RequesterPid, RequestInfo, continue)
    |
    send(GunConnectionPid, RequesterPid, RemainingRequestInfos)
  ];

send(_GunConnectionPid, _RequesterPid, []) -> [].

send_one(GunConnectionPid, RequesterPid, {RequestRef, RequestData}, close) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData, close),
  {ok, RequestPid} = request_server:start_link(RequesterPid, RequestRef),
  {StreamRef, {RequestPid, RequestRef}};

send_one(GunConnectionPid, RequesterPid, {RequestRef, RequestData}, continue) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  {ok, RequestPid} = request_server:start_link(RequesterPid, RequestRef),
  {StreamRef, {RequestPid, RequestRef}}.

requests_error(Streams, _Reason) when map_size(Streams) =:= 0 -> ok;

requests_error(Streams, Reason) ->
  [
    request_server:error(RequestPid, RequestRef, Reason) || {RequestPid, RequestRef} <- maps:values(Streams)
  ].
