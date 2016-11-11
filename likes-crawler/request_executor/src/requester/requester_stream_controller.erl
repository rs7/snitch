-module(requester_stream_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1, start_stream/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequesterId), {via, identifiable, {?MODULE, RequesterId}}).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterId) -> gen_server:start_link(?SERVER_NAME(RequesterId), ?MODULE, [], []).

start_stream(RequesterId, StreamRef, RequestId) ->
  gen_server:call(?SERVER_NAME(RequesterId), {start_stream, RequesterId, StreamRef, RequestId}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
