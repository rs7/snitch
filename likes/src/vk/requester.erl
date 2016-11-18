-module(requester).

%%% api
-export([start_link/0]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  lager:debug("Requester start", []),
  Pid = spawn_link(fun loop/0),
  {ok, Pid}.

%%%===================================================================
%%% internal
%%%===================================================================

loop() ->
  case requeue:get() of
    [] ->
      timer:sleep(100),
      loop();

    Queue -> work(Queue)
  end.

work(Queue) ->
  {Requests, Froms} = lists:unzip(Queue),
  Send = request:create(Requests),
  Recv = socket:process(Send),
  Responses = response:parse(Recv, length(Requests)),
  [process_result(From, Response, Request) || {From, Response, Request} <- lists:zip3(Froms, Responses, Requests)],
  loop().

process_result(From, {ok, Response}, _Request) ->
  metrics:notify(),
  requeue:reply(From, Response);

process_result(From, {error, _Reason}, Request) -> requeue:retry(From, Request).
