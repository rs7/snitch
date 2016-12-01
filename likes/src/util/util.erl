-module(util).

%%% api
-export([list_split/2]).

%%%===================================================================
%%% api
%%%===================================================================

list_split(List, Count) when length(List) < Count -> {List, []};
list_split(List, Count) -> lists:split(Count, List).
