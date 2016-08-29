-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, request/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  pid,
  monitor,
  requests
}).

-record(request, {
  from,
  request,
  status,
  data
}).

-record(response, {
  status,
  data
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

request(Connection, Request) -> gen_server:call(Connection, {request, Request}, infinity).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Pid} = connection_lib:open(),

  MonitorRef = monitor(process, Pid),

  {ok, #state{
    pid = Pid,
    monitor = MonitorRef,
    requests = #{}
  }}.

handle_call({request, Request}, From, #state{pid = Pid, requests = Requests} = State) ->
  StreamRef = connection_lib:request(Pid, Request),
  {noreply, State#state{requests = Requests#{StreamRef => #request{from = From, request = Request}}}};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, #state{pid = Pid, monitor = MonitorRef} = State) -> {ok, State};

handle_info({gun_up, Pid, http}, #state{pid = Pid} = State) -> {ok, State};

handle_info({gun_down, Pid, http, Reason, KilledStreams, UnprocessedStreams}, #state{pid = Pid} = State) -> {ok, State};

handle_info({gun_response, Pid, StreamRef, nofin, Status, _Headers}, #state{requests = Requests, pid = Pid} = State) ->
  Request =
  case IsFin of
    nofin -> {ok, State};
    fin -> {ok, State#state{requests = maps:remove(StreamRef, Requests)}}
  end;

handle_info({gun_data, Pid, StreamRef, IsFin, Data}, #state{pid = Pid} = State) -> {ok, State};

handle_info({gun_error, Pid, StreamRef, Reason}, #state{pid = Pid} = State) -> {ok, State};

handle_info({gun_error, Pid, Reason}, #state{pid = Pid} = State) -> {ok, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

from(From, StreamRef) -> maps:get(StreamRef, From).
