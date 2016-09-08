-module(logic_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_coworkers/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% internal
-export([call/2]).

-record(state, {requester_pid, call}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_coworkers(LogicPid, Coworkers) -> gen_server:call(LogicPid, {set_coworkers, Coworkers}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call({set_coworkers, [RequesterPid]}, _From, #state{requester_pid = undefined} = State) ->
  self() ! start,
  Call = create_call(RequesterPid),
  NewState = State#state{
    requester_pid = RequesterPid,
    call = Call
  },
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start, #state{call = Call} = State) ->
  Users = lists:seq(1052662, 1052662),
  Result = vk_process:process_users(Call, Users, [1868286, 3059512, 3079367, 51066050]),
  lager:info("~p", [Result]),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

call(RequesterPid, [RequestData]) -> [call(RequesterPid, RequestData)];

call(RequesterPid, [_ | _] = RequestDataList) ->
  {ok, Result} = util:parallel(
    {?MODULE, call},
    [[RequesterPid, RequestData] || RequestData <- RequestDataList]
  ),
  Result;

call(RequesterPid, RequestData) -> requester_server:call(RequesterPid, RequestData).

create_call(RequesterPid) -> fun(Args) -> call(RequesterPid, Args) end.
