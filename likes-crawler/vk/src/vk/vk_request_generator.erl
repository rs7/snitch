-module(vk_request_generator).

-behaviour(gen_server).

%% api
-export([start_link/0, get_request/0, commit_result/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {current_user}).

%%====================================================================
%% api
%%====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_request() -> gen_server:call(?MODULE, get_request).

commit_result(Result) -> gen_server:call(?MODULE, {commit_result, Result}).

%%====================================================================
%% gen_server
%%====================================================================

init([]) -> {ok, #state{current_user = 1}}.

handle_call(get_request, _From, State = #state{current_user = CurrentUser}) ->
  Result = {'friends.get', #{user_id => CurrentUser}},
  io:format("~B~n", [CurrentUser]),
  {reply, Result, State#state{current_user = CurrentUser + 1}};

handle_call({commit_result, _Result}, _From, State) -> {reply, ok, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
