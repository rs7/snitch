-module(requester).

-export([start_link/0]).

start_link() ->
  Pid = spawn_link(fun loop/0),
  {ok, Pid}.

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
  [requeue:reply(From, Response) || {From, Response} <- lists:zip(Froms, Responses)],
  loop().
