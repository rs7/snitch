-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, register_request/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  requester_pid,
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

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  {RequesterPid, _} = gproc:await(requester_server:gproc_key(WorkerId)),

  self() ! {send, 100},

  {ok, #state{
    requester_pid = RequesterPid,
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef,
    streams = #{}
  }}.

handle_call({register_request, RequestData, RequestPid}, _From,
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State) ->
  StreamRef = connection_lib:request(GunConnectionPid, RequestData),
  NewStreams = Streams#{StreamRef => RequestPid},
  NewState = State#state{streams = NewStreams},
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  {'DOWN', GunConnectionMonitorRef, process, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid, gun_connection_monitor_ref = GunConnectionMonitorRef} = State
) ->
  lager:warning("'DOWN' ~p", [Reason]),
  lager:debug("state ~p", [State]),
  {noreply, State};

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  lager:debug("gun_up"),
  {noreply, State};

handle_info(
  {gun_down, GunConnectionPid, http, Reason, KilledStreams, UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  lager:debug("gun_down"),
  lager:debug("state ~p", [State]),
  lager:debug("down params ~p ~p ~p", [Reason, KilledStreams, UnprocessedStreams]),
  self() ! {send, 100},
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  Proc = maps:get(StreamRef, Streams),
  request_server:set_status(Proc, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {Proc, NewStreams} = maps:take(StreamRef, Streams),
  request_server:set_status(Proc, Status),
  request_server:fin(Proc),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  Proc = maps:get(StreamRef, Streams),
  request_server:add_data(Proc, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {Proc, NewStreams} = maps:take(StreamRef, Streams),
  request_server:add_data(Proc, Data),
  request_server:fin(Proc),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  lager:warning("gun_error_stream ~p ~p", [StreamRef, Reason]),
  {noreply, State};

handle_info(
  {gun_error, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid} = State
) ->
  lager:warning("gun_error ~p", [Reason]),
  {noreply, State};

handle_info({send, Count}, #state{requester_pid = RequesterPid} = State) ->
  [
    request_server:start_link(RequesterPid, self())
    || _ <- lists:seq(1, Count)
  ],
  {noreply, State};

handle_info(Info, State) ->
  lager:warning("Unexpected message ~p", [Info]),
  {noreply, State}.

terminate(normal, _State) -> ok;

terminate(Reason, _State) ->
  lager:warning("terminate ~p", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
