-module(vk_request_pool).

-behaviour(gen_server).

%%% api
-export([start_link/0, get_request/0, result/3, error/3]).

%%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_id, queue}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_request() -> gen_server:call(?MODULE, get_request).

result(Result, Id, Time) -> gen_server:cast(?MODULE, {result, Result, Id, Time}).

error(Reason, Request, Id) -> gen_server:cast(?MODULE, {error, Reason, Request, Id}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) -> {ok, #state{
  last_id = 0,
  queue = []
}}.

handle_call(get_request, _From, State = #state{
  last_id = LastId,
  queue = []
}) ->
  Id = LastId + 1,
  Request = get_new_request(),
  {reply, {Request, Id}, State#state{
    last_id = Id
  }};

handle_call(get_request, _From, State = #state{
  last_id = LastId,
  queue = [Request | Remaining]
}) ->
  Id = LastId + 1,
  {reply, {Request, Id}, State#state{
    last_id = Id,
    queue = Remaining
  }};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({result, Result, Id, Time}, State) ->
  io:format("~B:~p | ~p~n", [Id, Result, Time]),
  {noreply, State};

handle_cast({error, Reason, Request, Id}, State = #state{
  queue = Queue
}) ->
  io:format("___Error___~n~p:~p~n~p~n", [Request, Id, Reason]),
  {noreply, State#state{
    queue = [Request | Queue]
  }};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

get_new_request() -> {'utils.getServerTime', #{}}.
