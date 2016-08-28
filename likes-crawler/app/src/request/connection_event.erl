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
  case gen_event:start_link() of
    {ok, Pid} ->
      case gen_event:add_handler(Pid, ?MODULE, []) of
        ok -> {ok, Pid};
        {error, Reason} -> {error, Reason};
        {'EXIT', Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.

request(Pid, Request) ->
  io:format("request1", []),
  gen_event:call(Pid, ?MODULE, {request, Request}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  ConnectionPid = connection_lib:open(),
  ConnectionMonitorRef = monitor(process, ConnectionPid),
  {ok, #state{
    pid = ConnectionPid,
    monitor = ConnectionMonitorRef
  }}.

handle_event(_Event, State) -> {ok, State}.

handle_call(
  {request, Request},
  #state{
    pid = ConnectionPid
  } = State
) ->
  io:format("request", []),
  StreamRef = connection_lib:request(ConnectionPid, Request),
  gen_event:add_handler(self(), request_event, StreamRef),
  {ok, ok, State};

handle_call(_Request, State) -> {ok, reply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) -> {ok, State};

handle_info({gun_up, ConnPid, Protocol}, State) -> {ok, State};

handle_info({gun_down, ConnPid, Protocol, Reason, KilledStreams, UnprocessedStreams}, State) -> {ok, State};

handle_info({gun_response, ConnPid, StreamRef, IsFin, Status, Headers} = Event, State) ->
  io:format("info", []),
  gen_event:notify(self(), Event),
  {ok, State};

handle_info({gun_data, ConnPid, StreamRef, IsFin, Data}, State) -> {ok, State};

handle_info({gun_error, ConnPid, StreamRef, Reason}, State) -> {ok, State};

handle_info({gun_error, ConnPid, Reason}, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
