-module(vk_requester).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection}).

%%%====================================================================
%%% api
%%%====================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
  case connect() of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      {ok, #state{connection = Pid}, 2000};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(do_request, _From, State = #state{connection = Connection}) ->
  Request = vk_request_generator:get_request(),
  Reply = case request(Connection, Request) of
    {ok, Result} -> {ok, Result};
    {error, Reason} -> {error, Reason}
  end,
  {reply, Reply, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(timeout, State) ->
  self() ! i_am_free,
  {noreply, State};

handle_info(i_am_free, State = #state{connection = Connection}) ->
  Request = vk_request_generator:get_request(),
  case request(Connection, Request) of
    {ok, Result} -> vk_request_generator:commit_result(Result);
    {error, Reason} -> io:format("Error: ~n~p~n~p~n", [Request, Reason])
  end,
  %%self() ! i_am_free,
  {noreply, State};

handle_info({'DOWN', _Reference, process, Pid, Reason}, State = #state{connection = Connection})
  when Pid == Connection ->
  {stop, Reason, State#state{connection = undefined}};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection = undefined}) -> ok;

terminate(_Reason, #state{connection = Connection}) ->
  shotgun:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

connect() -> shotgun:open("api.vk.com", 443, https).

request(Connection, Request) ->
  case shotgun_request(Connection, Request) of
    {ok, #{status_code := 200, body := Body}} -> parse_body(Body);
    {ok, #{status_code := StatusCode}} -> {error, {status_code, StatusCode}};
    {error, Reason} -> {error, {shotgun, Reason}}
  end.

shotgun_request(Connection, {Method, Params}) -> shotgun:post(
  Connection,
  ["/method/", Method],
  #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>},
  to_urlencoded(Params),
  #{timeout => 10000}
).

parse_body(Body) ->
  case jsone:try_decode(Body) of
    {ok, #{<<"response">> := Response}, _Remainings} -> {ok, {response, Response}};
    {ok, #{<<"error">> := #{<<"error_code">> := Code}}, _Remainings} -> {ok, {error, Code}};
    {error, Reason} -> {error, {json, Reason}}
  end.

%%%===================================================================
%%% util
%%%===================================================================

to_urlencoded(Params) -> string:join(
  [atom_to_list(Key) ++ "=" ++ to_list(Value) || {Key, Value} <- maps:to_list(Params)], "&"
).

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(List) when is_list(List) -> string:join(lists:map(fun to_list/1, List), ",").
