-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/0, get/0, reply/2, reject/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(REQUEST_QUEUE, <<"request">>).
-define(RESPONSE_QUEUE, <<"response">>).
-define(PREFETCH_COUNT, 100).

-record(state, {cache, channel, connection, consumer_tag}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() -> gen_server:call(?MODULE, get).

reply(Id, Reply) -> gen_server:cast(?MODULE, {reply, Id, Reply}).

reject(Id, Reason) -> gen_server:cast(?MODULE, {reject, Id, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?REQUEST_QUEUE, durable = true}),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = ?PREFETCH_COUNT}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} =
    amqp_channel:call(Channel, #'basic.consume'{queue = ?REQUEST_QUEUE}),

  State = #state{cache = [], channel = Channel, connection = Connection, consumer_tag = ConsumerTag},
  {ok, State}.

handle_call(get, _From, #state{cache = Cache} = State) ->
  NewCache = [],
  NewState = State#state{cache = NewCache},
  {reply, {ok, Cache}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({reply, {DeliveryTag, CorrelationId}, Reply}, #state{channel = Channel} = State) ->

  #'tx.select_ok'{} = amqp_channel:call(Channel, #'tx.select'{}),

  amqp_channel:cast(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?RESPONSE_QUEUE},
    #amqp_msg{
      payload = erlang:term_to_binary(Reply), props = #'P_basic'{delivery_mode = 2, correlation_id = CorrelationId}
    }
  ),

  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

  #'tx.commit_ok'{} = amqp_channel:call(Channel, #'tx.commit'{}),

  {noreply, State};

handle_cast({reject, {DeliveryTag, _CorrelationId}, _Reason}, #state{channel = Channel} = State) ->
  amqp_channel:cast(Channel, #'basic.reject'{delivery_tag = DeliveryTag}),
  {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  {
    #'basic.deliver'{delivery_tag = DeliveryTag},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  },
  #state{cache = Cache} = State
) ->
  lager:info("deliver: ~p", [Payload]),

  Request = {{DeliveryTag, CorrelationId}, erlang:binary_to_term(Payload)},

  NewCache = [Request | Cache],
  NewState = State#state{cache = NewCache},

  {noreply, NewState};

handle_info(_Info, State) ->
  lager:info("info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag}) ->
  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
