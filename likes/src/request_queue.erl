-module(request_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, get/2, ok/3, retry/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER_NAME(RequesterId), {via, identifiable, {?MODULE, RequesterId}}).

-define(REQUEST_QUEUE, <<"request">>).
-define(RESPONSE_QUEUE, <<"response">>).

-record(state, {connection, channel}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterId) -> gen_server:start_link(?SERVER_NAME(RequesterId), ?MODULE, [], []).

get(RequesterId, Count) -> gen_server:call(?SERVER_NAME(RequesterId), {get, Count}).

ok(RequesterId, RequestId, Result) -> gen_server:cast(?SERVER_NAME(RequesterId), {ok, RequestId, Result}).

retry(RequesterId, RequestId) -> gen_server:cast(?SERVER_NAME(RequesterId), {retry, RequestId}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?REQUEST_QUEUE, durable = true}),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?RESPONSE_QUEUE, durable = true}),

  NewState = #state{connection = Connection, channel = Channel},
  {ok, NewState}.

handle_call({get, Count}, _From, #state{channel = Channel} = State) ->
  Requests = get_requests(Channel, Count),
  {reply, {ok, Requests}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({ok, RequestId, Result}, #state{channel = Channel} = State) ->
  put_response(Channel,  , Result),
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = RequestId}),
  {noreply, State};

handle_cast({retry, RequestId}, #state{channel = Channel} = State) ->
  amqp_channel:cast(Channel, #'basic.reject'{delivery_tag = RequestId}),
  {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

get_requests(Channel, Count) -> get_requests(Channel, Count, []).

get_requests(_Channel, 0, Acc) -> lists:reverse(Acc);

get_requests(Channel, Count, Acc) ->
  case amqp_channel:call(Channel, #'basic.get'{queue = ?REQUEST_QUEUE}) of
    {
      #'basic.get_ok'{delivery_tag = DeliveryTag},
      #amqp_msg{payload = Payload, props = #'P_basic'{correlation_id = CorrelationId}}
    } ->
      Message = {DeliveryTag, erlang:binary_to_term(Payload)},
      get_requests(Channel, Count - 1, [Message | Acc]);

    #'basic.get_empty'{} -> get_requests(Channel, 0, Acc)
  end.

put_response(Channel, CorrelationId, Response) ->
  Payload = erlang:term_to_binary(Response),
  Publish = #'basic.publish'{exchange = <<"">>, routing_key = ?RESPONSE_QUEUE},
  amqp_channel:cast(
    Channel, Publish,
    #amqp_msg{payload = Payload, props = #'P_basic'{delivery_mode = 2, correlation_id = CorrelationId}}
  ).
