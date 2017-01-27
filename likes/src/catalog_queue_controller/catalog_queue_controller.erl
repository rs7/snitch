-module(catalog_queue_controller).

-behaviour(gen_server).

%%% api
-export([start_link/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {catalog_type, channel, connection, length_min, length_normal, queue, source, watch_timeout}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Params) -> gen_server:start_link(?MODULE, Params, []).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(#{
  queue := Queue,
  source := Source,
  catalog_type := CatalogType,
  watch_timeout := WatchTimeout,
  length_min := LengthMin,
  length_normal := LengthNormal
}) ->

  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  State = #state{
    catalog_type = CatalogType, channel = Channel, connection = Connection, length_min = LengthMin,
    length_normal = LengthNormal, queue = Queue, source = Source, watch_timeout = WatchTimeout
  },

  self() ! check,

  {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  check,
  #state{
    catalog_type = CatalogType, channel = Channel, length_min = LengthMin, length_normal = LengthNormal, queue = Queue,
    source = Source, watch_timeout = WatchTimeout
  } = State
) ->
  #'queue.declare_ok'{message_count = MessageCount} =
    amqp_channel:call(Channel, #'queue.declare'{passive = true, queue = Queue}),

  case MessageCount < LengthMin of
    true ->
      Generated = generate(Channel, Source, CatalogType, LengthNormal - MessageCount, []),

      [
        amqp_channel:call(
          Channel,
          #'basic.publish'{exchange = <<"">>, routing_key = Queue}, #amqp_msg{payload = integer_to_binary(Id)}
        )
        ||
        Id <- Generated
      ],

      case length(Generated) > 0 of
        true -> io:format("~p +~p~n", [Queue, length(Generated)]);
        false -> ok
      end,
      self() ! check;

    false -> erlang:send_after(WatchTimeout, self(), check)
  end,

  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{channel = Channel, connection = Connection}) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

generate(_Channel, _Queue, _CatalogType, Count, Acc) when Count =< 0 -> Acc;

generate(Channel, Queue, CatalogType, Count, Acc) ->
  case amqp_channel:call(Channel, #'basic.get'{queue = Queue, no_ack = true}) of

    {#'basic.get_ok'{}, #amqp_msg{payload = Payload}} ->
      Id = binary_to_integer(Payload),

      Result = catalog_rpc:call({CatalogType, Id}),

      generate(Channel, Queue, CatalogType, Count - length(Result), Acc ++ Result);

    #'basic.get_empty'{} -> Acc

  end.