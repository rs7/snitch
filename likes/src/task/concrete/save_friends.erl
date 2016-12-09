-module(save_friends).

-behaviour(gen_task).
-behaviour(gen_query_task).

%%% behaviour
-export([type/0, query/1, result/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query({User, Friends}) -> <<"save friends">>.

result(ok, _Args) -> [].
