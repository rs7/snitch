-module(api_rpc).

-behaviour(amqp_rpc).

%%% api
-export([call/1]).

%%% behaviour
-export([queue/0, serialize/1, deserialize/1]).

%%%===================================================================
%%% api
%%%===================================================================

call({Method, Params}) -> amqp_rpc:call(?MODULE, {Method, Params}).

%%%===================================================================
%%% behaviour
%%%===================================================================

queue() -> <<"snitch.api.request">>.

serialize(Request) -> term_to_binary(Request).

deserialize(Binary) -> binary_to_term(Binary).
