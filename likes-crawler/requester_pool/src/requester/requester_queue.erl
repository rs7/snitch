-module(requester_queue).

-behaviour(gen_server).

%%% api
-export([start_link/1, reserve/2, complete/2, retrieve/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER_NAME(RequesterId), {via, identifiable, {?MODULE, RequesterId}}).

-define(QUEUE, <<"requester_queue">>).

-record(state, {connection, channel}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterId) -> gen_server:start_link(?SERVER_NAME(RequesterId), ?MODULE, [], []).

reserve(RequesterId, Count) -> gen_server:call(?SERVER_NAME(RequesterId), {reserve, Count}).

complete(RequesterId, RequestId) -> gen_server:cast(?SERVER_NAME(RequesterId), {complete, RequestId}).

retrieve(RequesterId, RequestId) -> gen_server:cast(?SERVER_NAME(RequesterId), {retrieve, RequestId}).

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
  Messages = get_messages(Count, Channel),
  {reply, {ok, Messages}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({complete, RequestId}, #state{channel = Channel} = State) ->
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = RequestId}),
  lager:info("complete ~p", [RequestId]),
  {noreply, State};

handle_cast({retrieve, RequestId}, #state{channel = Channel} = State) ->
  amqp_channel:cast(Channel, #'basic.nack'{delivery_tag = RequestId}),
  lager:info("retrieve ~p", [RequestId]),
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

get_messages(Count, Channel) -> get_messages(Count, Channel, []).

get_messages(0, _Channel, Acc) -> lists:reverse(Acc);

get_messages(Count, Channel, Acc) ->
  case amqp_channel:call(Channel, #'basic.get'{queue = ?QUEUE}) of
    {#'basic.get_ok'{delivery_tag = DeliveryTag}, #amqp_msg{payload = Payload}} ->
      Message = {DeliveryTag, erlang:binary_to_term(Payload)},
      get_messages(Count - 1, Channel, [Message | Acc]);

    #'basic.get_empty'{} -> get_messages(0, Channel, Acc)
  end.
