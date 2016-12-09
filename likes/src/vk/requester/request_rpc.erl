-module(request_rpc).

-behaviour(gen_server).

%%% api
-export([start_link/0, call/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("../../amqp/amqp.hrl").

-record(state, {channel, connection, consumer_tag, from_dict}).

-define(PREFETCH_COUNT, 100).
-define(MAX_PRIORITY, 10).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Request) -> gen_server:call(?MODULE, {call, Request}, infinity).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel,
    #'queue.declare'{
      queue = ?REQUEST_QUEUE,
      auto_delete = true,
      arguments = [{<<"x-max-priority">>, short, ?MAX_PRIORITY}]
    }
  ),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?RESPONSE_QUEUE, auto_delete = true}),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = ?PREFETCH_COUNT}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} =
    amqp_channel:call(Channel, #'basic.consume'{queue = ?RESPONSE_QUEUE}),

  FromDict = dict:new(),

  State = #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag, from_dict = FromDict},
  {ok, State}.

handle_call({call, Request}, From, #state{channel = Channel, from_dict = FromDict} = State) ->

  Priority = amqp:priority(priority(Request)),

  CorrelationId = amqp:create_correlation_id(),
  Payload = amqp:serialize(Request),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?REQUEST_QUEUE},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId, priority = Priority}}
  ),

  NewFromDict = dict:store(CorrelationId, From, FromDict),

  NewState = State#state{from_dict = NewFromDict},
  {noreply, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  {
    #'basic.deliver'{delivery_tag = DeliveryTag},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  },
  #state{channel = Channel, from_dict = FromDict} = State
) ->
  Response = amqp:deserialize(Payload),

  From = dict:fetch(CorrelationId, FromDict),

  gen_server:reply(From, Response),

  ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

  NewFromDict = dict:erase(CorrelationId, FromDict),

  NewState = State#state{from_dict = NewFromDict},

  {noreply, NewState};

handle_info(#'basic.consume_ok'{}, State) -> {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) -> {noreply, State};

handle_info(_Info, State) ->
  lager:info("rpc info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag}) ->
  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

priority({'likes.getList', _Params}) -> 3;
priority({'photos.get', _Params}) -> 2;
priority({'photos.getAlbums', _Params}) -> 1;
priority({_Method, _Params}) -> 0.