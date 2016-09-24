-module(st).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STOP_TIMEOUT, 5000).
-define(TICK_TIMEOUT, 1000).
-define(TICK_MESSAGE, tick).
-define(STOP_MESSAGE, stop).

-record(state, {reason}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Reason) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Reason, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Reason) ->
  self() ! ?TICK_MESSAGE,
  erlang:send_after(?STOP_TIMEOUT, self(), ?STOP_MESSAGE),
  {ok, #state{reason = Reason}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(?TICK_MESSAGE, State) ->
  Message = sup:which_children(),
  io:format("~p~n", [Message]),

  erlang:send_after(?TICK_TIMEOUT, self(), ?TICK_MESSAGE),
  {noreply, State};

handle_info(?STOP_MESSAGE, #state{reason = Reason} = State) ->
  wor:stop(Reason),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
