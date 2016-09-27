-module(stat).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timeout, sources}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Timeout) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Timeout, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Timeout) ->
  self() ! timeout,
  NewSources = [
    {pool, fun requester_pool:get_size/0},
    {folsom,
      fun() ->
        Names = folsom_metrics:get_metrics(),
        Map = maps:from_list([{Name, folsom_metrics:get_metric_value(Name)} || Name <- Names]),
        {ok, Map}
      end
    }
  ],
  NewState = #state{timeout = Timeout, sources = NewSources},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(timeout, #state{timeout = Timeout, sources = Sources} = State) ->
  erlang:send_after(Timeout, self(), timeout),

  Result = maps:from_list(
    [
      fun({Name, Fun}) ->
        {ok, R} = Fun(),
        {Name, R}
      end(Source)
      ||
      Source <- Sources
    ]
  ),
  lager:info("~p", [Result]),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
