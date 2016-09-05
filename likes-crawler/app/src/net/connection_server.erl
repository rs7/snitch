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
  {ok, #state{
    streams = #{}
  }}.

%%%===================================================================
%%% set_requester_pid
%%%===================================================================

handle_call({set_requester_pid, RequesterPid}, _From, #state{requester_pid = undefined} = State) ->
  {ok, GunConnectionPid} = connection_lib:open(),

  GunConnectionMonitorRef = monitor(process, GunConnectionPid),

  NewState = State#state{
    requester_pid = RequesterPid,
    gun_connection_pid = GunConnectionPid,
    gun_connection_monitor_ref = GunConnectionMonitorRef
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
  all_error(Streams, {'DOWN', Reason}),
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

  request_server:error(RequestPid, {gun_stream_error, Reason}),

  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_up
%%%===================================================================

handle_info(
  {gun_up, GunConnectionPid, http},
  #state{gun_connection_pid = GunConnectionPid, requester_pid = RequesterPid} = State
) ->
  NewStreams = start_requests(GunConnectionPid, RequesterPid, ?STACK_SIZE),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

%%%===================================================================
%%% gun_down
%%%===================================================================

handle_info(
  {gun_down, GunConnectionPid, http, Reason, _KilledStreams, _UnprocessedStreams},
  #state{gun_connection_pid = GunConnectionPid, streams = Streams} = State
) ->
  all_error(Streams, {gun_down, Reason}),
  NewState = State#state{streams = #{}},
  {noreply, NewState};

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

start_requests(GunConnectionPid, RequesterPid, Count) ->
  case requester_server:reserve(RequesterPid, Count) of
    {ok, Requests} ->
      maps:from_list(
        lists:map(
          fun({RequestRef, RequestData}) ->
            StreamRef = connection_lib:request(GunConnectionPid, RequestData),
            {ok, RequestPid} = request_server:start_link(RequesterPid, RequestRef),
            {StreamRef, {RequestPid, RequestRef}}
          end,
          Requests
        )
      );

    {sleep, Timeout} ->
      timer:sleep(Timeout),
      start_requests(GunConnectionPid, RequesterPid, Count)
  end.

all_error(Streams, _Reason) when map_size(Streams) =:= 0 -> ok;

all_error(Streams, Reason) ->
  [
    request_server:error(RequestPid, Reason) || {RequestPid, _RequestRef} <- maps:values(Streams)
  ].
