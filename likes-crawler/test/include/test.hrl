-define(REQUEST, <<"GET /method/utils.getServerTime HTTP/1.1\nHost: api.vk.com\n\n">>).

-define(OUT(Format, Values), io:format("~p " ++ Format, [?MODULE | Values])).

-define(LOG, io:format("~B ~p~n", [erlang:monotonic_time(), ?FUNCTION_NAME])).

-define(PRINT(Values), io:format(
  lists:flatten([
    "~B", [" ~p" || _ <- Values], "~n"
  ]), [erlang:monotonic_time()] ++ Values)
).
