-module(user_1M_controller).

-behaviour(gen_queue_controller).

%%% behaviour
-export([init/0, generate/2]).

%%%===================================================================
%%% api
%%%===================================================================

init() ->
  #{
    queue => <<"snitch.user_1M">>,
    watch_timeout => 5000,
    length_min => 1,
    length_normal => 400000000
  }.

generate(_Channel, _Count) -> catalog_controller:get(e6, 0).
