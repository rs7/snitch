-module(requester_pool).

-behaviour(application).
-behaviour(supervisor).

%%% api
-export([get_size/0, set_size/1]).

%%% behaviour application
-export([start/2, stop/1]).

%%% behaviour supervisor
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

get_size() -> requester_pool_controller:get_requester_count().

set_size(Size) -> requester_pool_controller:set_requester_count(Size).

%%%===================================================================
%%% behaviour application
%%%===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%%%===================================================================
%%% behaviour supervisor
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 5, period => 1},

  {ok, Size} = application:get_env(size),

  Specifications = [
    #{
      id => request_queue,
      start => {request_queue, start_link, []},
      type => worker
    },
    #{
      id => requester_pool,
      start => {requester_pool_supervisor, start_link, [Size]},
      type => supervisor
    }
  ],

  {ok, {Strategy, Specifications}}.
