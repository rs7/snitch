-module(vk).

-export([call/1]).

call(Request) -> request_rpc:call(Request).
