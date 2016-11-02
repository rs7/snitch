-module(response_lib).

%%% api
-export([decode_body/1]).

%%%===================================================================
%%% api
%%%===================================================================

decode_body(Body) ->
  case jsone:try_decode(Body) of
    {ok, #{<<"response">> := Response}, <<>>} -> {ok, {response, Response}};
    {ok, #{<<"error">> := #{<<"error_code">> := ErrorCode}}, <<>>} -> {ok, {error, ErrorCode}};
    {ok, Result, <<>>} -> {error, {unexpected_result, Result}};
    {ok, _Result, Remainings} -> {error, {unexpected_remainings, Remainings}};
    {error, Reason} -> {error, Reason}
  end.
