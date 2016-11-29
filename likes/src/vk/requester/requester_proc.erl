-module(requester_proc).

%%% api
-export([start_link/0]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> spawn_link(fun loop/0).

%%%===================================================================
%%% internal
%%%===================================================================

loop() ->
  {Ids, Requests} = lists:unzip(get_requests()),
  Send = request:create(Requests),
  Recv = socket:process(Send),
  Responses = response:parse(Recv, length(Requests)),

  %%lists:foreach(fun process/1, lists:zip3(Froms, Requests, Responses)),

  loop().

get_requests() ->
  {ok, Result} = requester_queue:get(),
  case Result of
    [] ->
      timer:sleep(100),
      get_requests();
    Requests -> Requests
  end.
