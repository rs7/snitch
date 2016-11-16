-module(response).

-export([parse/2]).

parse(Responses, Count) ->
  [<<>> | L] = binary:split(Responses, [<<"HTTP/1.1 ">>, <<"\r\n\r\n">>], [global]),
  parse_packets(L, [], Count).

parse_packets([H, B | Rest], Result, Count) ->
  Packet = packet(H, B),
  parse_packets(Rest, [Packet | Result], Count);

parse_packets(_, Result, Count) -> fill(Result, Count - length(Result)).

fill(Result, 0) -> lists:reverse(Result);
fill(Result, Count) -> fill([fail(http_error) | Result], Count - 1).

packet(<<"200 OK", _/binary>>, Body) -> body(Body);
packet(_, _) -> fail(http_error).

body(Body) -> decode_result(jsone:try_decode(Body)).

decode_result({ok, Result, _Remainings}) -> result(Result);
decode_result({error, _Reason}) -> fail(json_error).

result(#{<<"response">> := Response}) -> success({response, Response});
result(#{<<"error">> := #{<<"error_code">> := ErrorCode}}) -> vk_error(ErrorCode);
result(_) -> fail(api_error).

vk_error(1) -> fail(vk_error);
vk_error(10) -> fail(vk_error);
vk_error(ErrorCode) -> success({error, ErrorCode}).

success(Result) -> {ok, Result}.

fail(Reason) -> {error, Reason}.
