-module(vk).

-behaviour(application).

%% API
-export([start/0, start_all/0, stop/0, single/1, multi/1, list/1, list/2, listCount/1]).

%% application
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() -> application:start(?MODULE).

start_all() -> application:ensure_all_started(?MODULE).

stop() -> application:stop(?MODULE).

single(Request) ->
  {response, Response} = vk_connection:request(Request),
  Response.

multi(Requests) -> lists:map(fun single/1, Requests).

list(Request) -> vk_list:get(Request).

list(Request, Count) -> vk_list:get(Request, Count).

listCount(Request) -> vk_list:getCount(Request).

%%====================================================================
%% application
%%====================================================================

start(_StartType, _StartArguments) -> vk_supervisor:start_link().

stop(_State) -> ok.

%%====================================================================
%% internal
%%====================================================================
