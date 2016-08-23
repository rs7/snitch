-module(worker_requester).

-behaviour(gen_server).

%%% api
-export([start_link/1, call/2, open_connection/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {worked_id}).

%%%====================================================================
%%% api
%%%====================================================================

start_link(WorkerId) -> gen_server:start_link(?MODULE, WorkerId, []).

call(Connection, Request) -> gen_server:call(Connection, {request, Request}, infinity).

open_connection(WorkerId) ->
  case shotgun:open("api.vk.com", 443, https) of
    {ok, Pid} ->
      link(Pid),
      gproc:reg_other({n, l, {worker_connection, WorkerId}}, Pid, Pid),
      {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  gproc:reg({n, l, {?MODULE, WorkerId}}, self()),
  {ok, #state{worked_id = WorkerId}}.

handle_call({request, Request}, _From, #state{worked_id = WorkerId} = State) ->
  {_Pid, Connection} = gproc:await({n, l, {worker_connection, WorkerId}}),
  Result = repeatable_request(Connection, Request, 1),
  {reply, Result, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

repeatable_request(Connection, Request, RetryNumber) ->
  lager:debug("~B/~p", [RetryNumber, Request]),
  case request(Connection, Request) of
    {result, Result} -> Result;
    {retry, Reason} ->
      error_warning(Reason, Request, RetryNumber),
      timer:sleep(retry_timeout(RetryNumber)),
      repeatable_request(Connection, Request, RetryNumber + 1)
  end.

request(Connection, Request) ->
  Result = shotgun_request(Connection, Request),
  case Result of
    {ok, #{status_code := 200, body := Body}} -> parse_body(Body);
    {ok, #{status_code := StatusCode}} -> {retry, {status_code, StatusCode}};
    {error, Reason} -> {retry, {shotgun, Reason}}
  end.

parse_body(Body) ->
  case jsone:try_decode(Body) of
    {ok, #{<<"response">> := Response}, _Remainings} -> {result, {response, Response}};
    {ok, #{<<"error">> := #{<<"error_code">> := Code}}, _Remainings} -> vk_error(Code);
    {error, Reason} -> {retry, {json, Reason}}
  end.

vk_error(1) -> {retry, {vk_error, 1}};
vk_error(10) -> {retry, {vk_error, 10}};
vk_error(Code) -> {result, {error, Code}}.

shotgun_request(Connection, {Method, Params}) -> shotgun:post(
  Connection,
  ["/method/", atom_to_list(Method)],
  #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>},
  to_urlencoded(Params),
  #{timeout => 10000}
).

retry_timeout(1) -> 10 * 1000;
retry_timeout(2) -> rand_seconds(30, 60);
retry_timeout(3) -> rand_seconds(60, 120);
retry_timeout(5) -> rand_seconds(120, 300).

error_warning(_Reason, _Request, TryNumber) when TryNumber < 5 -> ok;
error_warning(Reason, Request, TryNumber) -> lager:warning(
  "Request error ~B times~n~p~n~p", [TryNumber, Request, Reason]
).

%%%===================================================================
%%% util
%%%===================================================================

rand_seconds(From, To) -> (From + rand:uniform(To - From)) * 1000.

to_urlencoded(Params) -> string:join(
  [atom_to_list(Key) ++ "=" ++ to_list(Value) || {Key, Value} <- maps:to_list(Params)], "&"
).

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(List) when is_list(List) -> string:join(lists:map(fun to_list/1, List), ",").
