-module(vk).

-behaviour(application).

%% API
-export([single/1, multi/1, list/1, list/2, listCount/1, users/2, start/0, stop/0]).

%% application
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() -> application:ensure_all_started(?MODULE).

stop() -> application:stop(?MODULE).

single(Request) ->
  {response, Response} = vk_connection:request(Request),
  Response.

multi(Requests) -> lists:map(fun single/1, Requests).

list(Request) -> vk_list:get(Request).

list(Request, Count) -> vk_list:get(Request, Count).

listCount(Request) -> vk_list:getCount(Request).

users(Users, Params) -> vk_user:get(Users, Params).

%%====================================================================
%% application
%%====================================================================

start(_StartType, [ConnectionCount]) -> vk_supervisor:start_link(ConnectionCount).

stop(_State) -> ok.

%%====================================================================
%% internal
%%====================================================================
