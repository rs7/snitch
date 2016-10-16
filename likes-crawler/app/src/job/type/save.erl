-module(save).

-behaviour(gen_job).

%%% behavior
-export([process/2]).

%%%===================================================================
%%% behavior
%%%===================================================================

process(Priority, {Owner, Photo, Likers}) -> {ok, []}.
