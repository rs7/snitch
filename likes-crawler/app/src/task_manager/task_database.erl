-module(task_database).

%%% api
-export([create/0, start/0]).

-record(task, {ref, parent_ref, priority, type, status, timer_ref}).

%%%===================================================================
%%% api
%%%===================================================================

create() ->
  Nodes = [node()],
  mnesia:create_schema(Nodes),
  mnesia:start(),
  mnesia:create_table(task, [
    {attributes, record_info(fields, task)},
    {disc_copies, Nodes},
    {type, ordered_set}
  ]),
  mnesia:stop(),
  mnesia:info().

start() -> mnesia:wait_for_tables([task], 5000).
