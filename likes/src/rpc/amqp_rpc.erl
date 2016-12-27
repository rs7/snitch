-module(amqp_rpc).

%%% api
-export([call/2, behaviour_info/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{queue, 0}, {serialize, 1}, {deserialize, 1}];

behaviour_info(_Other) -> undefined.

call(Module, Request) ->

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  CorrelationId = create_correlation_id(),
  RequestPayload = Module:serialize(Request),

  RequestQueue = Module:queue(),
  ResponseQueue = <<"rpc.response.", CorrelationId/binary>>,

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel, #'queue.declare'{queue = ResponseQueue, arguments = [{<<"x-expires">>, signedint, 60000}]}
  ),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = RequestQueue},
    #amqp_msg{payload = RequestPayload, props = #'P_basic'{correlation_id = CorrelationId, reply_to = ResponseQueue}}
  ),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, #'basic.consume'{queue = ResponseQueue}),

  receive
    {
      #'basic.deliver'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = ResponsePayload, props = #'P_basic'{correlation_id = CorrelationId}}
    } ->
      Response = Module:deserialize(ResponsePayload),
      ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag})
  end,

  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),

  #'queue.delete_ok'{} = amqp_channel:call(Channel, #'queue.delete'{queue = ResponseQueue}),

  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),

  Response.

%%%===================================================================
%%% internal
%%%===================================================================

create_correlation_id() -> integer_to_binary(erlang:unique_integer([positive])).
