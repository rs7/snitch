-module(amqp).

%%% api
-export([serialize/1, deserialize/1, create_correlation_id/0, priority/1]).

%%%===================================================================
%%% api
%%%===================================================================

serialize(Term) -> term_to_binary(Term).

deserialize(Binary) -> binary_to_term(Binary).

create_correlation_id() -> integer_to_binary(erlang:unique_integer([positive])).

priority(Priority) -> integer_to_binary(Priority).
