-module(user_heap).

-behaviour(gen_server).

%%% api
-export([start_link/0, reserve/0, release/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME, {global, ?MODULE}).

-define(RESERVE_SIZE, 1000).

-record(state, {
  reserved = #{},
  current_user = 1,
  missed = []
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?SERVER_NAME, ?MODULE, [], []).

reserve() -> gen_server:call(?SERVER_NAME, reserve).

release(ReserveRef, Result) -> gen_server:call(?SERVER_NAME, {release, ReserveRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call(reserve, _From, State) -> {reply, ok, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
