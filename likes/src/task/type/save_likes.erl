-module(save_likes).

-behaviour(gen_task).
-behaviour(gen_query_task).

%%% behaviour
-export([type/0, query/1, result/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query({Owner, Photo, Likers}) -> <<"INSERT QUERY">>.

result(Result, _Args) -> [].
