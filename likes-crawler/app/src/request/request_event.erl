-module(request_event).

-behaviour(gen_event).

%%% behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {stream}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(StreamRef) ->
  io:format("request_event:init(~p)~n", [StreamRef]),
  {ok, #state{
    stream = StreamRef
  }}.

handle_event({gun_response, ConnPid, StreamRef, IsFin, Status, Headers}, State) ->
  io:format("gun_response ~p~n", [Status]),
  {ok, State};

handle_event({gun_data, ConnPid, StreamRef, IsFin, Data}, State) -> {ok, State};

handle_event({gun_error, ConnPid, StreamRef, Reason}, State) -> {ok, State}.

handle_call(_Request, State) -> {ok, reply, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
