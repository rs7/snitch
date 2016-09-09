-module(logic_server).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% internal
-export([call/1]).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  self() ! start,
  {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start, State) ->
  Users = lists:seq(1, 1000),
  Result = vk_process:process_users(fun call/1, Users, [1,34,197]),
  lager:info("~p", [Result]),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

call([RequestData]) -> [call(RequestData)];

call([_ | _] = RequestDataList) ->
  {ok, Result} = util:parallel(
    {?MODULE, call},
    [[RequestData] || RequestData <- RequestDataList]
  ),
  Result;

call(RequestData) -> heap_server:call(RequestData).
