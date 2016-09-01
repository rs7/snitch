-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, register_request/3, gproc_key/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  gun_connection_pid,
  gun_connection_monitor_ref,
  streams
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> gen_server:start_link(?MODULE, WorkerId, []).

register_request(ConnectionPid, RequestData, RequestPid) ->
  gen_server:call(ConnectionPid, {register_request, RequestData, RequestPid}).

gproc_key(WorkerId) -> {n, l, {connection, WorkerId}}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  gproc:reg(gproc_key(WorkerId)),

  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  {ok, #state{
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef,
    streams = #{}
  }}.

handle_call(
  {register_request, RequestData, RequestPid}, _From,
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  NewStreams = maps:put(connection_lib:request(GunConnectionPid, RequestData), {RequestPid, RequestData}, Streams),
  NewState = State#state{streams = NewStreams},
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

%%% DOWN

handle_info(
  {'DOWN', GunConnectionMonitorRef, process, GunConnectionPid, Reason},
  #state{
    gun_connection_pid = GunConnectionPid, gun_connection_monitor_ref = GunConnectionMonitorRef, streams = Streams
  } = State
) ->
  lager:warning("'DOWN' ~p", [Reason]),

  all_error(Streams),
  NewState = State#state{streams = #{}},

  {stop, {'DOWN', Reason}, NewState};

%%% gun_error

handle_info(
  {gun_error, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  lager:warning("gun_error ~p", [Reason]),

  all_error(Streams),
  NewState = State#state{streams = #{}},

  {stop, {gun_error, Reason}, NewState};

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  lager:warning("gun_error_stream ~p ~p", [StreamRef, Reason]),

  {{RequestPid, _RequestData}, NewStreams} = maps:take(StreamRef, Streams),

  request_server:error(RequestPid),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%% gun_up gun_down

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  {noreply, State};

handle_info(
  {gun_down, GunConnectionPid, http, _Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->

  NewStreams = maps:from_list([
    {connection_lib:request(GunConnectionPid, RequestData), Value}
    ||
    {_RequestPid, RequestData} = Value <- maps:values(Streams)
  ]),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%% gun_response gun_data

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, _RequestData} = maps:get(StreamRef, Streams),
  request_server:status(RequestPid, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, _RequestData}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:status(RequestPid, Status),
  request_server:fin(RequestPid),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, _RequestData} = maps:get(StreamRef, Streams),
  request_server:data(RequestPid, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, _RequestData}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:data(RequestPid, Data),
  request_server:fin(RequestPid),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

all_error(Streams) -> [request_server:error(RequestPid) || {RequestPid, _RequestData} <- maps:values(Streams)].
