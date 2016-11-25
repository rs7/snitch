-module(amqp).

%%% api
-export([connect/0, disconnect/1, queue_declare/2, queue_size/2, message_get/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% api
%%%===================================================================

connect() ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  {Connection, Channel}.

disconnect({Connection, Channel}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

queue_declare({_Connection, Channel}, Queue) ->
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Queue, durable = true}),
  ok.

queue_size({_Connection, Channel}, Queue) ->
  #'queue.declare_ok'{message_count = MessageCount} =
    amqp_channel:call(Channel, #'queue.declare'{queue = Queue, durable = true}),
  MessageCount.

message_get({_Connection, Channel}, Queue) ->
  case amqp_channel:call(Channel, #'basic.get'{queue = Queue}) of
    {
      #'basic.get_ok'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
    } ->
      {{DeliveryTag, CorrelationId}, erlang:binary_to_term(Payload)};

    #'basic.get_empty'{} -> undefined
  end.

message_put({_Connection, Channel}, Queue, Message) ->
  Payload = erlang:term_to_binary(Message),
  Publish = #'basic.publish'{exchange = <<"">>, routing_key = Queue},
  ok = amqp_channel:cast(
    Channel, Publish,
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId, delivery_mode = 2}}
  ).
