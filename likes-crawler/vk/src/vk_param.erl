-module(vk_param).

%% API exports
-export([to_value/1]).

%%====================================================================
%% API functions
%%====================================================================

to_value(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_value(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_value(List) when is_list(List) -> string:join(lists:map(fun to_value/1, List), ",").

%%====================================================================
%% Internal functions
%%====================================================================
