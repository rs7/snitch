-module(test).

%%% api
-export([start/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(REQUEST_QUEUE, <<"snitch.api.request">>).
-define(RESPONSE_QUEUE, <<"snitch.api.response.test">>).

%%%===================================================================
%%% api
%%%===================================================================

start(Request) ->

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel,
    #'queue.declare'{
      queue = ?RESPONSE_QUEUE,
      arguments = [
        {<<"x-expires">>, signedint, 60000}
      ]
    }
  ),

  OutputData = amqp:serialize(Request),
  CorrelationId = amqp:create_correlation_id(),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?REQUEST_QUEUE},
    #amqp_msg{payload = OutputData, props = #'P_basic'{correlation_id = CorrelationId, reply_to = ?RESPONSE_QUEUE}}
  ),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, #'basic.consume'{queue = ?RESPONSE_QUEUE}),

  recv(CorrelationId, Channel),

  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),

  ok.

recv(CorrelationId, Channel) ->
  receive
    {
      #'basic.deliver'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = InputData, props = #'P_basic'{correlation_id = CorrelationId}}
    } ->
      Response = amqp:deserialize(InputData),
      io:format("response: ~p~n", [Response]),
      ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag});

    {
      #'basic.deliver'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = InputData}
    } ->
      ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag});

    Message ->
      io:format("message: ~p~n", [Message]),
      recv(CorrelationId, Channel)
  end.