-define(IDENTIFIED_GPROC_KEY(Name, Id), {n, l, {Name, Id}}).

-define(IDENTIFIED_NAME(Name, Id), {via, gproc, ?IDENTIFIED_GPROC_KEY(Name, Id)}).
