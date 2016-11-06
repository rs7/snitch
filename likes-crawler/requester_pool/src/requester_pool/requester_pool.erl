-module(requester_pool).

-behaviour(application).
-behaviour(supervisor).

%%% api
-export([start_child/1, terminate_child/1]).

%%% behaviour application
-export([start/2, stop/1]).

%%% behaviour supervisor
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_child(RequesterId) -> supervisor:start_child(?MODULE, [RequesterId]).

terminate_child(RequesterId) -> supervisor:terminate_child(?MODULE, requester:whereis(RequesterId)).

%%%===================================================================
%%% behaviour application
%%%===================================================================

start(_StartType, _StartArgs) ->
  Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  {ok, Size} = application:get_env(size),
  start_children(Size),

  Result.

stop(_State) -> ok.

%%%===================================================================
%%% behaviour supervisor
%%%===================================================================

init([]) ->
  Strategy = #{strategy => simple_one_for_one, intensity => 5, period => 1},

  Specifications = [
    #{
      id => requester,
      start => {requester, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.

%%%===================================================================
%%% internal
%%%===================================================================

start_children(0) -> ok;

start_children(Count) ->
  start_child(Count),
  start_children(Count - 1).
