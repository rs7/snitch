-define(
  DBG(Pattern, Values),
  io:format(?MODULE_STRING ++ ":" ++ Pattern ++ "~n", Values)
).
