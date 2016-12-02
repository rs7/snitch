-module(user_queue).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("../amqp/amqp.hrl").

-record(state, {channel, connection}).

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

  #'queue.declare_ok'{} = amqp_channel:call(
    Channel,
    #'queue.declare'{
      queue = ?USER_QUEUE,
      durable = true,
      arguments = [{<<"x-queue-mode">>, binary, <<"lazy">>}]
    }
  ),

  State = #state{channel = Channel, connection = Connection},
  {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) ->
  lager:info("user_queue info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
