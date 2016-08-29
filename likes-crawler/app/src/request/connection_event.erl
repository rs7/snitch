-module(connection_event).

-behaviour(gen_event).

%%% api
-export([start_link/0, request/2]).

%%% behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  pid,
  monitor
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  {ok, Pid} = gen_event:start_link(),
  ok = gen_event:add_handler(Pid, ?MODULE, []),
  {ok, Pid}.

request(Pid, Request) ->
  StreamRef = gen_event:call(Pid, ?MODULE, {request, Request}),
  ok = gen_event:add_handler(Pid, request_event, StreamRef).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, ConnectionPid} = connection_lib:open(),
  ConnectionMonitorRef = monitor(process, ConnectionPid),
  {ok, #state{
    pid = ConnectionPid,
    monitor = ConnectionMonitorRef
  }}.

handle_event(_Event, State) -> {ok, State}.

handle_call(get_connection_pid, #state{pid = ConnectionPid} = State) -> {ok, ConnectionPid, State};

handle_call({request, Request}, #state{pid = ConnectionPid} = State) ->
  StreamRef = connection_lib:request(ConnectionPid, Request),
  {ok, StreamRef, State};

handle_call(_Request, State) -> {ok, reply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) -> {ok, State};

handle_info({gun_up, ConnPid, Protocol}, State) -> {ok, State};

handle_info({gun_down, ConnPid, Protocol, Reason, KilledStreams, UnprocessedStreams}, State) -> {ok, State};

handle_info({gun_response, ConnPid, StreamRef, IsFin, Status, Headers} = Event, State) ->
  gen_event:notify(self(), Event),
  {ok, State};

handle_info({gun_data, ConnPid, StreamRef, IsFin, Data} = Event, State) ->
  gen_event:notify(self(), Event),
  {ok, State};

handle_info({gun_error, ConnPid, StreamRef, Reason} = Event, State) ->
  gen_event:notify(self(), Event),
  {ok, State};

handle_info({gun_error, ConnPid, Reason}, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
