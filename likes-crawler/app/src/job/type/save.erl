-module(save).

-behaviour(gen_job).

%%% behavior
-export([process/2]).

%%%===================================================================
%%% behavior
%%%===================================================================

process(Priority, {Liker, Owner, Photo}) ->
  lager:info("id~B ♥ photo~B_~B", [Liker, Owner, Photo]),
  {ok, []}.
