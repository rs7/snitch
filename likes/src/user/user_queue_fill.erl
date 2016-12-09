-module(user_queue_fill).

%%% api
-export([exec/1]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("../amqp/amqp.hrl").

%%%===================================================================
%%% api
%%%===================================================================

exec(Users) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'tx.select_ok'{} = amqp_channel:call(Channel, #'tx.select'{}),

  ok = publish(Channel, Users),

  #'tx.commit_ok'{} = amqp_channel:call(Channel, #'tx.commit'{}),

  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

publish(_Channel, []) -> ok;

publish(Channel, [User | Remaining]) ->
  Payload = amqp:serialize(User),
  CorrelationId = amqp:create_correlation_id(),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?USER_QUEUE},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  ),

  publish(Channel, Remaining).
