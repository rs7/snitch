-module(init).

%%% api
-export([start/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%day 1 1
%%user_1M 400 1
%%user_10K 0..100 1
%%user_100 0..100 1
%%user 0..100 1
%%album 0+ 1
%%photo 0..1000 1
%%like 0..1000 100

%%%===================================================================
%%% api
%%%===================================================================

start() ->

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),

  ok.
