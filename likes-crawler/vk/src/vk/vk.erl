-module(vk).

-behaviour(application).

%% api
-export([start/0, stop/0]).

%% application
-export([start/2, stop/1]).

%%====================================================================
%% api
%%====================================================================

start() -> application:ensure_all_started(?MODULE).

stop() -> application:stop(?MODULE).

%%====================================================================
%% application
%%====================================================================

start(_StartType, [RequestersCount]) -> vk_supervisor:start_link(RequestersCount).

stop(_State) -> ok.

%%====================================================================
%% internal
%%====================================================================
