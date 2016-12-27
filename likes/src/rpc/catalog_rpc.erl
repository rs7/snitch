-module(catalog_rpc).

-behaviour(amqp_rpc).

%%% api
-export([call/1]).

%%% behaviour
-export([queue/0, serialize/1, deserialize/1]).

%%%===================================================================
%%% api
%%%===================================================================

call({Type, Id}) -> amqp_rpc:call(?MODULE, {Type, Id}).

%%%===================================================================
%%% behaviour
%%%===================================================================

queue() -> <<"snitch.catalog.request">>.

serialize({Type, Id}) -> jsone:encode(#{type => Type, id => Id}).

deserialize(Binary) -> jsone:decode(Binary).
