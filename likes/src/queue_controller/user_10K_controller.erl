-module(user_10K_controller).

-behaviour(gen_queue_controller).

%%% behaviour
-export([init/0, generate/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% api
%%%===================================================================

init() ->
  #{
    queue => <<"snitch.user_10K">>,
    watch_timeout => 3000,
    length_min => 10,
    length_normal => 50
  }.

generate(Channel, Count) -> generate(Channel, Count, []).

generate(_Channel, Count, Acc) when length(Acc) >= Count -> Acc;

generate(Channel, Count, Acc) ->

  {#'basic.get_ok'{}, #amqp_msg{payload = Payload}} =
    amqp_channel:call(Channel, #'basic.get'{queue = <<"snitch.user_1M">>}),

  User1MId = binary_to_term(Payload),

  Result = catalog_controller:get(e4, User1MId),

  generate(Channel, Count - length(Result), Acc ++ Result).