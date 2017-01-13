-module(balancer).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(QUEUE, <<"snitch.balancer">>).

-record(state, {channel, connection, consumer_tag}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE, durable = true}),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, #'basic.consume'{queue = ?QUEUE}),

  State = #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag},
  {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
    {#'basic.deliver'{delivery_tag = DeliveryTag}, #amqp_msg{payload = Payload}}, #state{channel = Channel} = State
) ->
  Data = amqp_deserialize(Payload),

  io:format("balance: ~p~n", [Data]),

  ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag}) ->
  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

amqp_deserialize(Binary) -> binary_to_term(Binary).
