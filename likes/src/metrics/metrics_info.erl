-module(metrics_info).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TIMEOUT, 10000).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  self() ! print,
  {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(print, State) ->
  erlang:send_after(?TIMEOUT, self(), print),

  {ok, {Duration, Values}} = metrics_data:get(),

  process(Duration, Values),

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

process(Duration, Values) ->


  {Format, Args} = lists:unzip([
    process(Name, Value, Duration) || {Name, Value} <- Values
  ]),

  lager:info(
    "--------------~n" ++ string:join(Format, "~n"),
    lists:append(Args)
  ),

  ok.

process(Name, Value, Duration) -> {"~.3f ~p/sec", [Value / Duration, Name]}.
