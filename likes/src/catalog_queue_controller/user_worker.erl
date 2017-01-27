-module(user_worker).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(USER, <<"snitch.user">>).

-record(state, {channel, connection}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  State = #state{channel = Channel, connection = Connection},

  self() ! process,

  {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(process, #state{channel = Channel} = State) ->

  case amqp_channel:call(Channel, #'basic.get'{queue = ?USER, no_ack = true}) of

    {#'basic.get_ok'{}, #amqp_msg{payload = Payload}} ->
      User = binary_to_integer(Payload),
      io:format("process: ~p~n", [User]);

    #'basic.get_empty'{} ->
      io:format("empty ---------------------------------------------------~n", [])

  end,

  erlang:send_after(1, self(), process),

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
