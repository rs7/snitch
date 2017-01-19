-module(gen_queue_controller).

-behaviour(gen_server).

%%% api
-export([behaviour_info/1, start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {channel, connection, module, length_min, length_normal, queue, watch_timeout}).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{init, 0}, {generate, 2}];

behaviour_info(_Other) -> undefined.

start_link(Module) -> gen_server:start_link(?MODULE, Module, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Module) ->
  #{
    queue := Queue,
    watch_timeout := WatchTimeout,
    length_min := LengthMin,
    length_normal := LengthNormal
  } = Module:init(),

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  State = #state{
    channel = Channel, connection = Connection, module = Module, length_min = LengthMin, length_normal = LengthNormal,
    queue = Queue, watch_timeout = WatchTimeout
  },

  self() ! check,

  {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  check,
  #state{
    channel = Channel, module = Module, length_min = LengthMin, length_normal = LengthNormal, queue = Queue,
    watch_timeout = WatchTimeout
  } = State
) ->
  #'queue.declare_ok'{message_count = MessageCount} =
    amqp_channel:call(Channel, #'queue.declare'{passive = true, queue = Queue}),

  case MessageCount < LengthMin of
    true ->
      [
        begin
          ok = amqp_channel:call(
            Channel, #'basic.publish'{exchange = <<"">>, routing_key = Queue}, #amqp_msg{payload = Message}
          )
        end
        ||
        Message <- Module:generate(Channel, LengthNormal - MessageCount)
      ];

    false -> ok
  end,

  erlang:send_after(WatchTimeout, self(), check),

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
