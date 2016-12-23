-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/1, reply/3, reject/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(QUEUE, <<"snitch.api.request">>).
-define(SERVER_NAME(Id), {via, identifiable, {?MODULE, Id}}).

-record(request, {correlation_id, data, delivery_tag, id, reply_to}).
-record(state, {cache, channel, connection, consumer_tag, in_progress}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Id) -> gen_server:start_link(?SERVER_NAME(Id), ?MODULE, [], []).

get(Id) -> gen_server:call(?SERVER_NAME(Id), get).

reply(Id, RequestId, Reply) -> gen_server:cast(?SERVER_NAME(Id), {reply, RequestId, Reply}).

reject(Id, RequestId, Reason) -> gen_server:cast(?SERVER_NAME(Id), {reject, RequestId, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE}),
  #'basic.qos_ok'{} = amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 200}),
  #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:call(Channel, #'basic.consume'{queue = ?QUEUE}),

  Cache = [],
  InProgress = #{},

  State = #state{
    cache = Cache, channel = Channel, connection = Connection, consumer_tag = ConsumerTag, in_progress = InProgress
  },
  {ok, State}.

handle_call(get, _From, #state{cache = Cache, in_progress = InProgress} = State) ->
  {Requests, NewCache} = split_cache(Cache, 100),

  NewInProgress = maps:merge(InProgress, maps:from_list([{Request#request.id, Request} || Request <- Requests])),

  Result = [{RequestId, Data} || #request{data = Data, id = RequestId} <- Requests],

  NewState = State#state{cache = NewCache, in_progress = NewInProgress},
  {reply, {ok, Result}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({reply, RequestId, Response}, #state{channel = Channel, in_progress = InProgress} = State) ->

  {Request, NewInProgress} = maps:take(RequestId, InProgress),

  #request{correlation_id = CorrelationId, delivery_tag = DeliveryTag, reply_to = ReplyTo} = Request,

  Payload = amqp:serialize(Response),

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel, #'queue.declare'{queue = ReplyTo, arguments = [{<<"x-expires">>, signedint, 60000}]}
  ),

  ok = amqp_channel:call(
    Channel,
    #'basic.publish'{exchange = <<"">>, routing_key = ReplyTo},
    #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
  ),

  ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),

  NewState = State#state{in_progress = NewInProgress},

  {noreply, NewState};

handle_cast({reject, RequestId, _Reason}, #state{cache = Cache, in_progress = InProgress} = State) ->

  {Request, NewInProgress} = maps:take(RequestId, InProgress),

  NewCache = Cache ++ [Request],

  NewState = State#state{cache = NewCache, in_progress = NewInProgress},

  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
    {
      #'basic.deliver'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId, reply_to = ReplyTo}}
    },
    #state{cache = Cache} = State
) ->
  Data = amqp:deserialize(Payload),
  Id = make_ref(),

  Request = #request{
    correlation_id = CorrelationId, data = Data, delivery_tag = DeliveryTag, id = Id, reply_to = ReplyTo
  },

  NewCache = [Request | Cache],
  NewState = State#state{cache = NewCache},

  {noreply, NewState};

handle_info(#'basic.consume_ok'{}, State) -> {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) -> {noreply, State};

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

split_cache(Cache, Count) when length(Cache) > Count -> lists:split(length(Cache) - Count, Cache);
split_cache(Cache, _Count) -> {Cache, []}.
