-module(identifiable).

%%% api
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-define(KEY(Name, Id), {n, l, {Name, Id}}).

%%%===================================================================
%%% api
%%%===================================================================

register_name({Name, Id}, Pid) -> gproc:register_name(?KEY(Name, Id), Pid).

unregister_name({Name, Id}) -> gproc:unregister_name(?KEY(Name, Id)).

whereis_name({Name, Id}) -> gproc:whereis_name(?KEY(Name, Id)).

send({Name, Id}, Message) -> gproc:send(?KEY(Name, Id), Message).
