-module(vk_connection).

-behaviour(gen_server).

%% API
-export([start_link/0, request/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

request(Request) -> gen_server:call(?MODULE, {request, Request}).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
  State = #state{connection = connect()},
  {ok, State}.

handle_call({request, Request}, _From, State = #state{connection = Connection}) ->
  Reply = request(Connection, Request),
  {reply, Reply, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

connect() ->
  {ok, Connection} = shotgun:open("api.vk.com", 443, https),
  Connection.

request(Connection, {Method, Params}) ->
  {ok, #{body := Body, status_code := 200}} = shotgun:post(
    Connection,
    "/method/" ++ atom_to_list(Method),
    #{<<"Content-Type">> => <<"application/x-www-form-urlencoded">>},
    to_urlencoded(Params),
    #{}
  ),
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
