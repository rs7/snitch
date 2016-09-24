-module(sup).

-behaviour(supervisor).

%%% api
-export([start_link/1, which_children/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Reason) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Reason).

which_children() -> supervisor:which_children(?MODULE).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(Reason) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => worker_supervisor,
      restart => transient,
      start => {wor, start_link, []},
      type => worker
    },
    #{
      id => stat_server,
      start => {st, start_link, [Reason]},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.
