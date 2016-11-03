-module(request_queue).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {connection, channel, queue}).

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

  Exchange = <<"requester_exchange">>,
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange}),

  Queue = <<"requester_queue">>,
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Queue, durable = true}),

  RoutingKey = <<"requester_key">>,
  Binding = #'queue.bind'{queue = Queue, exchange = Exchange, routing_key = RoutingKey},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

  Payload = <<"request">>,
  Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

  NewState = #state{connection = Connection, channel = Channel, queue = Queue},
  {ok, NewState}.

handle_call({get}, _From, #state{channel = Channel, queue = Queue} = State) ->
  {
    #'basic.get_ok'{delivery_tag = DeliveryTag},
    #amqp_msg{payload = Payload}
  } = amqp_channel:call(Channel, #'basic.get'{queue = Queue}),
  {reply, ok, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({retry, DeliveryTag}, #state{channel = Channel} = State) ->
  amqp_channel:call(Channel, #'basic.nack'{delivery_tag = DeliveryTag}),
  {noreply, State};

handle_cast({confirm, DeliveryTag}, #state{channel = Channel} = State) ->
  amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),
  {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
