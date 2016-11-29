-module(vk).

-export([call/1]).

call(Request) ->
  {ok, Ref} = work_queue:add(Request, 1, self()),
  {ok, Reply} = work_queue:recv(Ref, infinity),
  Reply.
