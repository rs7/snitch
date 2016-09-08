-module(gun_close_test).

%%% api
-export([start/0]).

%%%===================================================================
%%% api
%%%===================================================================

start() ->
  application:ensure_all_started(gun),
  {ok, Pid} = connection_lib:open(),
  [connection_lib:request(Pid, mock:get_request_data()) || _ <- lists:seq(1, 4)],
  gun:get(Pid, <<"/method/utils.getServerTime">>, [{<<"Connection">>, <<"close">>}]),
  listen().

listen() ->
  Msg = receive M -> M end,
  io:format("~p~n", [Msg]),
  listen().
