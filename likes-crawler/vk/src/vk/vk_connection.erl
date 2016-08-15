-module(vk_connection).

-behaviour(gen_statem).

%%% api
-export([start_link/1, call/2, start_shotgun_connection/1]).

%%% gen_statem
-export([init/1, terminate/3, code_change/4, handle_event/4]).

-define(CALLBACK_MODE, handle_event_function).

-record(data, {connection, request}).

%%%====================================================================
%%% api
%%%====================================================================

start_link(WorkerId) -> gen_statem:start_link(?MODULE, WorkerId, []).

call(Connection, Request) -> gen_statem:call(Connection, {request, Request}, infinity).

start_shotgun_connection(WorkerId) ->
  case shotgun:open("api.vk.com", 443, https) of
    {ok, Pid} ->
      gproc:reg({n, l, {shotgun_connection, WorkerId}}, Pid),
      timer:apply_after(5000, erlang, exit, [Pid, ok]),
      {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% gen_statem
%%%===================================================================

init(WorkerId) ->
  gproc:reg({n, l, {?MODULE, WorkerId}}, self()),
  gproc:monitor({n, l, {shotgun_connection, WorkerId}}, follow),
  {?CALLBACK_MODE, {unknow, free}, #data{}}.

%%handle_event({call, From}, {request, Request}, {connected, free}, #data{connection = Connection} = Data) ->
%%  case State of
%%    connected ->
%%      case request(Connection, Request) of
%%        {ok, Result} -> {keep_state, Data, [{reply, From, Result}]};
%%        {error, Reason} ->
%%          lager:warning("Request ~p error ~p", [Request, Reason]),
%%          {next_state, connected_in_progress, Data}
%%      end;
%%    disconnected ->
%%      {keep_state, Data, [postpone]};
%%    connected_in_progress -> {keep_state, Data, [{reply, From, {error, not_allowed}}]};
%%    disconnected_in_progress -> {keep_state, Data, [{reply, From, {error, not_allowed}}]}
%%  end;
%%
%%handle_event({call, From}, {request, Request}, State, #data{connection = Connection} = Data) ->
%%  {keep_state, Data, [{reply, From, {error, not_allowed}}]};

handle_event(info, {gproc, registered, _Ref, _Key}, {_ConnectMode, BusyMode}, Data) ->
  lager:info("connected"),
  {next_state, {connected, BusyMode}, Data};

handle_event(info, {gproc, unreg, _Ref, _Key}, {_ConnectMode, BusyMode}, Data) ->
  lager:info("disconnected"),
  {next_state, {disconnected, BusyMode}, Data}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_OldVsn, State, Data, _Extra) -> {?CALLBACK_MODE, State, Data}.

%%%===================================================================
%%% internal
%%%===================================================================

request(Connection, Request) ->
  Result = shotgun_request(Connection, Request),
  case Result of
    {ok, #{status_code := 200, body := Body}} -> parse_body(Body);
    {ok, #{status_code := StatusCode}} -> {error, {status_code, StatusCode}};
    {error, Reason} -> {error, {shotgun, Reason}}
  end.

shotgun_request(Connection, {Method, Params}) -> shotgun:post(
  Connection,
  ["/method/", atom_to_list(Method)],
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

try_timeout(1) -> 10 * 1000;
try_timeout(2) -> 60 * 1000;
try_timeout(3) -> 120 * 1000;
try_timeout(5) -> (120 + random:uniform(180)) * 1000.

%%%===================================================================
%%% util
%%%===================================================================

to_urlencoded(Params) -> string:join(
  [atom_to_list(Key) ++ "=" ++ to_list(Value) || {Key, Value} <- maps:to_list(Params)], "&"
).

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(List) when is_list(List) -> string:join(lists:map(fun to_list/1, List), ",").
