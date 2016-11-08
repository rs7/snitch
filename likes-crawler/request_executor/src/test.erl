-module(test).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(PUT_TIMEOUT, 10).
-define(QUEUE, <<"requester_queue">>).

-record(state, {connection, channel}).

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

  self() ! put,

  NewState = #state{connection = Connection, channel = Channel},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(put, #state{channel = Channel} = State) ->
  erlang:send_after(?PUT_TIMEOUT, self(), put),
  put_message(Channel),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
  amqp_channel:close(Channel),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

put_message(Channel) ->
  CorrelationId = integer_to_binary(erlang:unique_integer()),
  Message = {'utils.getServerTime', #{}},
  Payload = erlang:term_to_binary(Message),
  Publish = #'basic.publish'{exchange = <<"">>, routing_key = ?QUEUE},
  amqp_channel:cast(
    Channel, Publish,
    #amqp_msg{payload = Payload, props = #'P_basic'{delivery_mode = 2, correlation_id = CorrelationId}}
  ).
