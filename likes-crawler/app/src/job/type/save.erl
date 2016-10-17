-module(save).

-behaviour(gen_job).
-behaviour(gen_query_job).

%%% behaviour
-export([type/0, query/1]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query({Owner, Photo, Likers}) -> <<"INSERT QUERY">>.
