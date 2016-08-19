-module(user_counter).

-behaviour(gen_server).

%%% api
-export([start_link/0, get/0, fail/1, success/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, failed}).

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

init([]) -> {ok, #state{user = 1, failed = []}}.

handle_call(get, _From, #state{user = User, failed = []} = State) ->
  {reply, User, State#state{user = next_user(User)}};

handle_call(get, _From, #state{failed = [User | Remaining]} = State) ->
  {reply, User, State#state{failed = Remaining}};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({fail, User}, #state{failed = Failed} = State) ->
  {noreply, State#state{failed = [User | Failed]}};

handle_cast({success, _User}, State) -> {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

next_user(370000000) -> 1;
next_user(User) -> User + 1.
