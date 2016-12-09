-module(save_relation).

-behaviour(gen_task).
-behaviour(gen_query_task).

%%% behaviour
-export([type/0, query/1, result/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query({User, Relation}) -> <<"save relation">>.

result(ok, _Args) -> [].
