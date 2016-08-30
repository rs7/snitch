-module(connection_server).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  pid,
  monitor,
  streams
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Pid} = connection_lib:open(),

  Monitor = monitor(process, Pid),

  self() ! {send, 5},

  {ok, #state{
    pid = Pid,
    monitor = Monitor,
    streams = #{}
  }}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', Monitor, process, Pid, Reason}, #state{pid = Pid, monitor = Monitor} = State) ->
  lager:warning("'DOWN' ~p", [Reason]),
  lager:debug("state ~p", [State]),
  {noreply, State};

handle_info({gun_up, Pid, http}, #state{pid = Pid} = State) ->
  lager:debug("gun_up"),
  {noreply, State};

handle_info({gun_down, Pid, http, Reason, KilledStreams, UnprocessedStreams}, #state{pid = Pid} = State) ->
  lager:debug("gun_down"),
  lager:debug("state ~p", [State]),
  {noreply, State};

handle_info({gun_response, Pid, Stream, nofin, Status, _Headers}, #state{pid = Pid, streams = Streams} = State) ->
  Proc = maps:get(Stream, Streams),
  request_server:set_status(Proc, Status),
  {noreply, State};

handle_info({gun_response, Pid, Stream, fin, Status, _Headers}, #state{pid = Pid, streams = Streams} = State) ->
  {Proc, NewStreams} = maps:take(Stream, Streams),
  request_server:set_status(Proc, Status),
  request_server:fin(Proc),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

handle_info({gun_data, Pid, Stream, nofin, Data}, #state{pid = Pid, streams = Streams} = State) ->
  Proc = maps:get(Stream, Streams),
  request_server:add_data(Proc, Data),
  {noreply, State};

handle_info({gun_data, Pid, Stream, fin, Data}, #state{pid = Pid, streams = Streams} = State) ->
  {Proc, NewStreams} = maps:take(Stream, Streams),
  request_server:add_data(Proc, Data),
  request_server:fin(Proc),
  NewState = State#state{streams = NewStreams},
  {noreply, NewState};

handle_info({gun_error, Pid, Stream, Reason}, #state{pid = Pid} = State) ->
  lager:warning("gun_error_stream ~p ~p", [Stream, Reason]),
  {noreply, State};

handle_info({gun_error, Pid, Reason}, #state{pid = Pid} = State) ->
  lager:warning("gun_error ~p", [Reason]),
  {noreply, State};

handle_info({send, Count}, #state{pid = Pid, streams = Streams} = State) ->
  NewStreams = maps:from_list([
    {connection_lib:request(Pid, Request), Proc} || {Request, Proc} <- requests_buffer:get_requests(Count)
  ]),
  lager:debug("send ~p ~p", [Count, NewStreams]),
  NewState = State#state{streams = maps:merge(Streams, NewStreams)},
  {noreply, NewState};

handle_info(Info, State) ->
  lager:warning("Unexpected message ~p", [Info]),
  {noreply, State}.

terminate(normal, _State) -> ok;

terminate(Reason, _State) ->
  lager:warning("terminate ~p", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
