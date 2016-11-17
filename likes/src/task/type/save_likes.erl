-module(save_likes).

-behaviour(gen_task).
-behaviour(gen_query_task).

%%% behaviour
-export([type/0, query/1, result/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query({Owner, Photo, Likers}) ->
  R = list_to_binary(io_lib:format("~w", [{Owner, Photo, Likers}])),
  <<"INSERT QUERY ", R/binary>>.

result(_Result, _Args) -> [].
