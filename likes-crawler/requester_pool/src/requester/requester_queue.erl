-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, reserve/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER_NAME(RequesterRef), {via, identifiable, {?MODULE, RequesterRef}}).

-define(QUEUE, <<"requester_queue">>).

-record(state, {connection, channel}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef) -> gen_server:start_link(?SERVER_NAME(RequesterRef), ?MODULE, [], []).

reserve(RequesterRef, Count) -> gen_server:call(?SERVER_NAME(RequesterRef), {reserve, Count}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE, durable = true}),

  NewState = #state{connection = Connection, channel = Channel},
  {ok, NewState}.

handle_call({reserve, Count}, _From, #state{channel = Channel} = State) ->
  Messages = get_requests(Count, Channel),
  {reply, {ok, Messages}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%handle_cast({retry, DeliveryTag}, #state{channel = Channel} = State) ->
%%  amqp_channel:call(Channel, #'basic.nack'{delivery_tag = DeliveryTag}),
%%  {noreply, State};
%%
%%handle_cast({confirm, DeliveryTag}, #state{channel = Channel} = State) ->
%%  amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),
%%  {noreply, State};

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

get_requests(Count, Channel) -> get_requests(Count, Channel, []).

get_requests(0, _Channel, Acc) -> lists:reverse(Acc);

get_requests(Count, Channel, Acc) ->
  case amqp_channel:call(Channel, #'basic.get'{queue = ?QUEUE}) of
    {#'basic.get_ok'{delivery_tag = DeliveryTag}, #amqp_msg{payload = Payload}} ->
      RequestData = erlang:binary_to_term(Payload),
      RequestRef = DeliveryTag,
      RequestInfo = {RequestRef, RequestData},
      get_requests(Count - 1, Channel, [RequestData | Acc]);

    #'basic.get_empty'{} -> get_requests(0, Channel, Acc)
  end.
