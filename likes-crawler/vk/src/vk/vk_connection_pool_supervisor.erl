-module(vk_connection_pool_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0, get_children/0]).

%% supervisor
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
  Result = {ok, PID} = supervisor:start_child(?MODULE, []),
  io:format("Start child ~p~n", [PID]),
  Result.

get_children() -> supervisor:which_children(?MODULE).

%%====================================================================
%% supervisor
%%====================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 1, period => 5},
  ChildSpecification = #{id => vk_connection, start => {vk_connection, start_link, []}},
  {ok, {Strategy, [ChildSpecification]}}.
