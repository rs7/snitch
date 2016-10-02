-module(conveyor_list).

%%% api
-export([create_list/0, extract/1]).

%%%===================================================================
%%% api
%%%===================================================================

create_list() ->
  LastUser = get_last_user(),
  Step = util:ceil(LastUser / 1000),
  (next(1, LastUser, Step))().

extract([Element | Next]) -> {Element, Next()}.

%%%===================================================================
%%% internal
%%%===================================================================

next(Nth, _LastUser, Step) when Nth =:= Step + 1 -> fun() -> undefined end;

next(Nth, LastUser, Step) -> fun() -> [
  generate_list(Nth, LastUser, Step)
  |
  next(Nth + 1, LastUser, Step)
] end.

generate_list(Nth, LastUser, Step) -> lists:seq(Nth, LastUser, Step).

get_last_user() -> 387000000.
