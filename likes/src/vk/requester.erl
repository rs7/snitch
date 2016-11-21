-module(requester).

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
  {Requests, Froms} = lists:unzip(queue()),
  Send = request:create(Requests),
  Recv = socket:process(Send),
  Responses = response:parse(Recv, length(Requests)),

  lists:foreach(fun process/1, lists:zip3(Froms, Requests, Responses)),

  loop().

queue() ->
  case requeue:get() of
    [] ->
      timer:sleep(100),
      queue();
    Queue -> Queue
  end.

process({From, _Request, {ok, Response}}) -> requeue:reply(From, Response);
process({From, Request, {error, _Reason}}) -> requeue:retry(From, Request).
