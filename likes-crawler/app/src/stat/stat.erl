-module(stat).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timeout}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Timeout) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Timeout, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Timeout) ->
  self() ! info_metrics,
  NewState = #state{timeout = Timeout},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(info_metrics, #state{timeout = Timeout} = State) ->
  erlang:send_after(Timeout, self(), info_metrics),
  lager:info("~p", [metrics()]),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

metrics() ->
  Metrics = [complete, pool, queue, call, request, retry],
  Tags = [type],
  lists:append(
    [{Metric, folsom_metrics:get_metric_value(Metric)} || Metric <- Metrics],
    [{Tag, folsom_metrics:get_metrics_value(Tag)} || Tag <- Tags]
  ).
