-module(vk_request_pool).

-behaviour(gen_server).

%%% api
-export([start_link/0, get_request/0, commit_result/1]).

%%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_id, data, in_progress, repeat}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_request() -> gen_server:call(?MODULE, get_request).

commit_result(Result) -> gen_server:call(?MODULE, {result, Result}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) -> {ok, #state{
  last_id = 0,
  data = #{},
  in_progress = [],
  repeat = []
}}.

handle_call(get_request, _From, State = #state{
  last_id = LastId, data = Data, in_progress = InProgress, repeat = []
}) ->
  Id = LastId + 1,
  TryNumber = 0,
  Request = get_new_request(),
  {reply, {Request, {Id, TryNumber}}, State#state{
    last_id = Id,
    data = Data#{Id => {Request, TryNumber}},
    in_progress = [Id | InProgress]
  }};

handle_call(get_request, _From, State = #state{
  data = Data#{Id := {Request, TryNumber}},
  in_progress = InProgress,
  repeat = [Id | RemainingFailed]
}) ->
  NewTryNumber = TryNumber + 1,
  {reply, {Request, {Id, NewTryNumber}}, State#state{
    data = Data#{Id => {Request, NewTryNumber}},
    repeat = RemainingFailed,
    in_progress = [Id | InProgress]
  }};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({result, {{ok, Result}, {Id, TryNumber}}}, State = #state{
  data = Data#{Id := {Request, TryNumber}},
  in_progress = InProgress
}) ->
  io:format("~p ~p~n", [Id, Result]),
  {noreply, State#state{
    data = Data#{Id => {Request, NewTryNumber}},
    repeat = RemainingFailed,
    in_progress = [Id | InProgress]
  }};

handle_cast({result, {{error, Reason}, {Id, TryNumber}}}, State = #state{
  data = Data#{Id := {Request, TryNumber}},
  repeat = Repeat
}) ->
  io:format("Error~p~n~p~p~n", [Request, Id, Reason]),
  NewTryNumber = TryNumber + 1,
  {noreply, State#state{
    data = Data#{Id => {Request, NewTryNumber}},
    repeat = [Id | Repeat]
  }};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

get_new_request() -> {'utils.getServerTime', #{}}.

result({ok, Result}) -> ok;
result({error, Reason}) -> io:format("Error~n~p~n", [Reason]).
