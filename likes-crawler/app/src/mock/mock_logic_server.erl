-module(mock_logic_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_requester_pid/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {requester_pid}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_requester_pid(LogicPid, RequesterPid) -> gen_server:call(LogicPid, {set_requester_pid, RequesterPid}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call({set_requester_pid, RequesterPid}, _From, #state{requester_pid = undefined} = State) ->

  [
    spawn(
      fun Do() ->
        {ok, _Result} = requester_server:call(RequesterPid, mock:get_random_request_data()),
        Do()
      end
    )
    || _ <- lists:seq(1, 150)
  ],

  NewState = State#state{
    requester_pid = RequesterPid
  },
  {reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
