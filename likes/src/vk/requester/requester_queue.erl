-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/1, reply/3, reject/4]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(REQUEST_QUEUE, <<"request">>).
-define(RESPONSE_QUEUE, <<"response">>).
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

  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?REQUEST_QUEUE, auto_delete = true}),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?RESPONSE_QUEUE, auto_delete = true}),

  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = ?PREFETCH_COUNT}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} =
    amqp_channel:call(Channel, #'basic.consume'{queue = ?REQUEST_QUEUE}),

  State = #state{cache = [], channel = Channel, connection = Connection, consumer_tag = ConsumerTag},
  {ok, State}.

handle_call(get, _From, #state{cache = Cache} = State) ->
  {Result, NewCache} = util:list_split(Cache, 100),

  lager:info("getted: ~p", [length(Result)]),

  NewState = State#state{cache = NewCache},
  {reply, {ok, Result}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({reply, {DeliveryTag, CorrelationId}, Reply}, #state{channel = Channel} = State) ->

  Payload = erlang:term_to_binary(Reply),

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

  NewCache = [{Id, Request} | Cache],

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
  Request = {{DeliveryTag, CorrelationId}, erlang:binary_to_term(Payload)},

  NewCache = [Request | Cache],
  NewState = State#state{cache = NewCache},

  {noreply, NewState};

handle_info(_Info, State) ->
  lager:info("queue info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection, consumer_tag = ConsumerTag}) ->
  #'basic.cancel_ok'{} = amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = ConsumerTag}),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
