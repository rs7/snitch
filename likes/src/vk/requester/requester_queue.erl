-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/1, reply/3, reject/4]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("../../amqp/amqp.hrl").

-define(PREFETCH_COUNT, 200).

-define(SERVER_NAME(Id), {via, identifiable, {?MODULE, Id}}).

-record(state, {cache, channel, connection, consumer_tag, id}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Id) -> gen_server:start_link(?SERVER_NAME(Id), ?MODULE, [], []).

get(Id) -> gen_server:call(?SERVER_NAME(Id), get).

reply(Id, RequestId, Reply) -> gen_server:cast(?SERVER_NAME(Id), {reply, RequestId, Reply}).

reject(Id, RequestId, Request, Reason) -> gen_server:cast(?SERVER_NAME(Id), {reject, RequestId, Request, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = ?PREFETCH_COUNT}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} =
    amqp_channel:call(Channel, #'basic.consume'{queue = ?REQUEST_QUEUE}),

  Cache = [],

  State = #state{cache = Cache, channel = Channel, connection = Connection, consumer_tag = ConsumerTag},
  {ok, State}.

handle_call(get, _From, #state{cache = Cache} = State) ->
  {Result, NewCache} = split_cache(Cache, 100),

  NewState = State#state{cache = NewCache},
  {reply, {ok, Result}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({reply, {DeliveryTag, CorrelationId}, Response}, #state{channel = Channel} = State) ->

  Payload = amqp:serialize(Response),

  #'tx.select_ok'{} = amqp_channel:call(Channel, #'tx.select'{}),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ?RESPONSE_QUEUE},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  ),

  ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

  #'tx.commit_ok'{} = amqp_channel:call(Channel, #'tx.commit'{}),

  metrics_data:inc(reply),

  {noreply, State};

handle_cast({reject, Id, Request, _Reason}, #state{cache = Cache} = State) ->

  NewCache = Cache ++ [{Id, Request}],

  NewState = State#state{cache = NewCache},

  metrics_data:inc(reject),

  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  {
    #'basic.deliver'{delivery_tag = DeliveryTag},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  },
  #state{cache = Cache} = State
) ->
  Request = {{DeliveryTag, CorrelationId}, amqp:deserialize(Payload)},

  NewCache = [Request | Cache],
  NewState = State#state{cache = NewCache},

  {noreply, NewState};

handle_info(#'basic.consume_ok'{}, State) -> {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) -> {noreply, State};

handle_info(_Info, State) ->
  lager:info("queue info: ~p", [_Info]),
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

split_cache(Cache, Count) when length(Cache) > Count -> lists:split(length(Cache) - Count, Cache);
split_cache(Cache, _Count) -> {Cache, []}.
