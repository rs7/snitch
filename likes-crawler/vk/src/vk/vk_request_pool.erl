-module(vk_request_pool).

-behaviour(gen_server).

%%% api
-export([start_link/0, get/0, result/2, error/2]).

%%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_id, queue, in_progress}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() -> gen_server:call(?MODULE, get).

result(Result, Id) -> gen_server:cast(?MODULE, {result, Result, Id}).

error(Reason, Id) -> gen_server:cast(?MODULE, {error, Reason, Id}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) -> {ok, #state{
  last_id = 0,
  queue = [],
  in_progress = #{}
}}.

handle_call(get, _From, State = #state{
  last_id = LastId,
  queue = [],
  in_progress = InProgress
}) ->
  Id = LastId + 1,
  Request = get_new_request(friends),
  {reply, {Request, Id}, State#state{
    last_id = Id,
    in_progress = InProgress#{Id => Request}
  }};

handle_call(get, _From, State = #state{
  last_id = LastId,
  queue = [Request | Remaining],
  in_progress = InProgress
}) ->
  Id = LastId + 1,
  {reply, {Request, Id}, State#state{
    last_id = Id,
    queue = Remaining,
    in_progress = InProgress#{Id => Request}
  }};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({result, Result, Id}, State = #state{
  in_progress = InProgress
}) ->
  case InProgress of
    #{Id := Request} ->
      lager:info("~B:~p | ~40P~n", [Id, Request, Result, 10]),
      {noreply, State#state{
        in_progress = maps:remove(Id, InProgress)
      }};
    _ -> {noreply, State}
  end;


handle_cast({error, Reason, Id}, #state{
  queue = Queue,
  in_progress = InProgress
} = State) ->
  case InProgress of
    #{Id := Request} ->
      lager:warning("~B:~p | ~n~p~n", [Id, Request, Reason]),
      {noreply, State#state{
        queue = [Request | Queue],
        in_progress = maps:remove(Id, InProgress)
      }};
    _ -> {noreply, State}
  end;

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

get_new_request(time) -> {'utils.getServerTime', #{}};

get_new_request(friends) -> {'friends.get', #{user_id => rand:uniform(370000000)}}.
