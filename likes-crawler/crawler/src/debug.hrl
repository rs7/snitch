-ifdef(debug).
  -define(
    DBG(Pattern, Values),
    io:format(?MODULE_STRING ++ ":" ++ Pattern ++ "~n", Values)
  ).
-else.
  -define(DBG(Pattern, Values), ok).
-endif.
