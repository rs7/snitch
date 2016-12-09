-module(test_amqp).

%%% api
-export([start/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(QUEUE, <<"test.big_size">>).

%%%===================================================================
%%% api
%%%===================================================================

start() ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel,
    #'queue.declare'{queue = ?QUEUE, durable = true, arguments = [{<<"x-queue-mode">>, longstr, <<"lazy">>}]}
  ),

  publish(Channel, 500000000),

  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),

  ok.

publish(_Channel, 0) -> ok;

publish(Channel, User) ->
  Payload = amqp:serialize(User),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?QUEUE},
    #amqp_msg{payload = Payload}
  ),

  publish(Channel, User - 1).
