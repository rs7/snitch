-define(IDENTIFIED_GPROC_KEY(Name, Id), {n, l, {Name, Id}}).

-define(IDENTIFIED_NAME(Name, Id), {via, gproc, ?IDENTIFIED_GPROC_KEY(Name, Id)}).

-define(IDENTIFIED_PID(Name, Id), gproc:lookup_pid(?IDENTIFIED_GPROC_KEY(Name, Id))).
