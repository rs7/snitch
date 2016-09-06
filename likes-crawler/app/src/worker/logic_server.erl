-module(logic_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_coworkers/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% internal
-export([call/2]).

-record(state, {requester_pid}).

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
  NewState = State#state{
    requester_pid = RequesterPid
  },
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start, #state{requester_pid = RequesterPid} = State) ->
  [
    spawn(
      fun Do() ->
        {ok, _Result} = requester_server:call(RequesterPid, mock:get_random_request_data()),
        Do()
      end
    )
    || _ <- lists:seq(1, 150)
  ],
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

call(Call, [_ | _] = Requests) -> rpc:parallel_eval([
  {?MODULE, call, [Call, Request]} || Request <- Requests
]);

call(Call, RequestData) -> Call(RequestData).
