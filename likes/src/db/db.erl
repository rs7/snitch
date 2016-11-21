-module(db).

%%% api
-export([run/1]).

%%%===================================================================
%%% api
%%%===================================================================

run(<<"SELECT user">>) -> rand:uniform(400000000);

run(<<"INSERT likes", R/binary>>) ->
  %io:format("likes: ~p~n", [R]),
  ok.
