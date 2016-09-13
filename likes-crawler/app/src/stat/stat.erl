-module(stat).

%%% api
-export([]).

%%%===================================================================
%%% api
%%%===================================================================

metrics() -> #{
  memory => erlang:memory(),
  port_count => erlang:system_info(port_count),
  process_count => erlang:system_info(process_count)
}.
