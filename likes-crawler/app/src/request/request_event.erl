-module(request_event).

-behaviour(gen_event).

%%% behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {stream, status, body}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(StreamRef) ->
  {ok, #state{
    stream = StreamRef,
    body = <<>>
  }}.

handle_event({gun_response, _ConnPid, StreamRef, IsFin, Status, _Headers}, #state{stream = RequestStreamRef} = State)
  when RequestStreamRef == StreamRef ->
  check_fin(IsFin),
  {ok, State#state{
    status = Status
  }};

handle_event({gun_data, _ConnPid, StreamRef, IsFin, Data}, #state{stream = RequestStreamRef, body = Body} = State)
  when RequestStreamRef == StreamRef ->
  check_fin(IsFin),
  {ok, State#state{
    body = <<Body/binary, Data/binary>>
  }};

handle_event({gun_error, _ConnPid, StreamRef, Reason}, #state{stream = RequestStreamRef} = State)
  when RequestStreamRef == StreamRef ->
  lager:info("error ~p", [Reason]),
  self() ! fin,
  {ok, State}.

handle_call(_Request, State) -> {ok, reply, State}.

handle_info(fin, #state{status = Status, body = Body} = State) ->
  lager:info("finish ~p ~p", [Status, Body]),
  {ok, State};

handle_info(_Info, State) -> {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

check_fin(fin) -> self() ! fin;
check_fin(nofin) -> ok.
