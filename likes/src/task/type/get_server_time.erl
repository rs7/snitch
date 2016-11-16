-module(get_server_time).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request([]) ->
  {
    'utils.getServerTime',
    #{
      v => '5.53'
    }
  }.

response({response, Timestamp}, _Context) ->
  io:format("timestamp ~p~n", [Timestamp]),
  [].
