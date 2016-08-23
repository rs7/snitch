-module(process).

-behaviour(supervisor).

%%% api
-export([start_link/0, process_func/0]).

%%% behaviour
-export([init/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

process_func() -> fun process/1.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  Strategy = #{strategy => one_for_all, intensity => 1, period => 5},

  Specifications = [
    #{
      id => counter,
      start => {counter, start_link, []},
      type => worker
    }
  ],

  {ok, {Strategy, Specifications}}.

%%%===================================================================
%%% internal
%%%===================================================================

process(Call) ->
  case counter:get() of
    {user, User} ->
      lager:debug("User ~B", [User]),
      case vk_api:is_user_active(User, Call) of
        true -> vk_api:get_likes(User, Call);
        false -> ok
      end,
      next;
    undefined -> stop
  end.
