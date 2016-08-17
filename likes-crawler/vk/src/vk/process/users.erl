-module(users).

-behaviour(gen_server).

%%% api
-export([start_link/0, get/0, fail/1, success/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, failed, in_progress}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() -> gen_server:call(?MODULE, get).

fail(User) -> gen_server:cast(?MODULE, {fail, User}).

success(User) -> gen_server:cast(?MODULE, {success, User}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) -> {ok, #state{user = 1, failed = [], in_progress = #{}}}.

handle_call(get, _From, #state{user = User, failed = [], in_progress = InProgress} = State) ->
  {reply, User, State#state{user = next_user(User), in_progress = InProgress#{User => 1}}};

handle_call(get, _From, #state{failed = [User | Remaining], in_progress = InProgress} = State) ->
  {reply, User, State#state{failed = Remaining, in_progress = InProgress#{User => 1}}};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({fail, User}, #state{failed = Failed, in_progress = InProgress} = State) ->
  {noreply, State#state{failed = [User | Failed], in_progress = maps:remove(User, InProgress)}};

handle_cast({success, User}, #state{in_progress = InProgress} = State) ->
  {noreply, State#state{in_progress = maps:remove(User, InProgress)}};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

next_user(User) -> User + 1.
