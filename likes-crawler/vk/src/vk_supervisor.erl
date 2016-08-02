-module(vk_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor
%%====================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 0, period => 1},
  Specification = #{id => vk_connection, start => {vk_connection, start_link, []}},
  {ok, {Strategy, [Specification]}}.
