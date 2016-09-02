-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_requester_pid/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STACK_SIZE, 100).

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

set_requester_pid(ConnectionPid, RequesterPid) -> gen_server:call(ConnectionPid, {set_requester_pid, RequesterPid}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  {ok, #state{
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef,
    streams = #{}
  }}.

handle_call({set_requester_pid, RequesterPid}, _From, State) ->
  NewState = State#state{requester_pid = RequesterPid},
  {reply, ok, NewState};

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
  all_error(Streams, {gun_DOWN, Reason}),
  NewState = State#state{streams = #{}},

  {stop, {'DOWN', Reason}, NewState};

%%%===================================================================
%%% gun_error
%%%===================================================================

handle_info(
  {gun_error, GunConnectionPid, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  all_error(Streams, {gun_error, Reason}),
  NewState = State#state{streams = #{}},

  {stop, {gun_error, Reason}, NewState};

handle_info(
  {gun_error, GunConnectionPid, StreamRef, Reason},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, _RequestInfo}, NewStreams} = maps:take(StreamRef, Streams),

  request_server:error(RequestPid, {gun_error_stream, Reason}),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams, requester_pid = RequesterPid} = State
) ->

  Unprocessed = maps:values(Streams),

  {ok, New} = requester_server:reserve(RequesterPid, ?STACK_SIZE - length(Unprocessed)),

  NewStreams = maps:from_list(lists:map(
    fun({_RequestRef, RequestData} = RequestInfo) ->
      StreamRef = connection_lib:request(GunConnectionPid, RequestData),
      {ok, RequestPid} = request_server:start_link(RequesterPid, RequestInfo),
      {StreamRef, {RequestPid, RequestInfo}}
    end, Unprocessed ++ New
  )),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_down
%%%===================================================================

handle_info(
  {gun_down, GunConnectionPid, http, Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  case map_size(Streams) of
    0 -> ok;
    UnprocessedCount -> lager:warning("unprocessed streams ~p count | down reason ~p", [UnprocessedCount, Reason])
  end,
  {noreply, State};

%%%===================================================================
%%% gun_response
%%%===================================================================

handle_info(
  {gun_response, GunConnectionPid, StreamRef, nofin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, _RequestInfo} = maps:get(StreamRef, Streams),
  request_server:status(RequestPid, Status),
  {noreply, State};

handle_info(
  {gun_response, GunConnectionPid, StreamRef, fin, Status, _Headers},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, _RequestInfo}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:status(RequestPid, Status),
  request_server:fin(RequestPid),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_data
%%%===================================================================

handle_info(
  {gun_data, GunConnectionPid, StreamRef, nofin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {RequestPid, _RequestInfo} = maps:get(StreamRef, Streams),
  request_server:data(RequestPid, Data),
  {noreply, State};

handle_info(
  {gun_data, GunConnectionPid, StreamRef, fin, Data},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  {{RequestPid, _RequestInfo}, NewStreams} = maps:take(StreamRef, Streams),
  request_server:data(RequestPid, Data),
  request_server:fin(RequestPid),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

all_error(Streams, Reason) ->
  [
    request_server:error(RequestPid, Reason) || {RequestPid, _RequestInfo} <- maps:values(Streams)
  ].
