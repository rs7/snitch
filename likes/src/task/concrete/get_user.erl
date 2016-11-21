-module(get_user).

-behaviour(gen_task).
-behaviour(gen_query_task).

%%% behaviour
-export([type/0, query/1, result/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> query.

query([]) -> <<"SELECT user">>.

result(User, _Args) -> [{get_albums, User}].
