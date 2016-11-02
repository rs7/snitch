-module(queue).

%%% api
-export([init/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% api
%%%===================================================================

init() ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  Exchange = <<"requester_exchange">>,
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange}),

  Queue = <<"requester_queue">>,
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),

  RoutingKey = <<"requester_key">>,
  Binding = #'queue.bind'{queue = Queue, exchange = Exchange, routing_key = RoutingKey},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

  Payload = <<"request">>,
  Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

  ok.

get(Count) -> ok.

retrieve() -> ok.
