-module(vk).

-export([call/1]).

call(Request) -> requeue:exec(Request).
