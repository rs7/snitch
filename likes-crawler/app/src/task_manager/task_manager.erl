-module(task_manager).

%%% api
-export([start/0, reserve/2, release/2, create_database/0]).

%%%===================================================================
%%% api
%%%===================================================================

%%Мятеж на старом судне,
%%Как чья-то напасть.
%%Ворвалась в наши будни
%%Мятежная власть.

start() -> task_database:start().

reserve(Type, Count) -> ok.

release(JobRef, {ok, Result}) -> ok;

release(JobRef, retrieve) -> ok.

create_database() -> task_database:create().
