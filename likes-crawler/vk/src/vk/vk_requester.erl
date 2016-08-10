-module(vk_requester).

-behaviour(gen_server).

%% api
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection}).

%%====================================================================
%% api
%%====================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
  case shotgun:open("api.vk.com", 443, https) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      erlang:send_after(1000, self(), free),
      {ok, #state{connection = Pid}};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call({request, Request}, _From, State = #state{connection = Connection}) ->
  Reply = case do_request(Connection, Request) of
    {ok, Result} -> {result, Result};
    {error, Reason} -> {retry, Reason}
  end,
  {reply, Reply, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', _Reference, process, Pid, Reason}, State = #state{connection = Connection})
  when Pid == Connection ->
  {stop, Reason, State#state{connection = undefined}};

handle_info(free, State = #state{connection = Connection}) ->
  Request = vk_request_generator:get_request(),
  case do_request(Connection, Request) of
    {ok, Result} ->
      vk_request_generator:commit_result(Result),
      erlang:send_after(0, self(), free);
    {error, Reason} ->
      io:format("Error: ~n~p~n~p~n", [Request, Reason]),
      erlang:send_after(1000, self(), free)
  end,

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection = undefined}) -> ok;

terminate(_Reason, #state{connection = Connection}) ->
  shotgun:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

do_request(Connection, Request) ->
  case shotgun_request(Connection, Request) of
    {ok, #{body := Body, status_code := 200}} -> {ok, parse_body(Body)};
    {ok, #{status_code := StatusCode}} -> {error, {status_code, StatusCode}};
    {error, Reason} -> {error, Reason}
  end.

shotgun_request(Connection, {Method, Params}) -> shotgun:post(
  Connection,
  "/method/" ++ atom_to_list(Method),
  #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>},
  to_urlencoded(Params),
  #{timeout => 10000}
).

parse_body(Body) ->
  case jsone:decode(Body) of
    #{<<"response">> := Response} -> {response, Response};
    #{<<"error">> := #{<<"error_code">> := Code}} -> {error, Code}
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
