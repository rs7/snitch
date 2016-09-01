-module(mock).

%%% api
-export([get_request_data/0, get_fail_request_data/0]).

%%%===================================================================
%%% api
%%%===================================================================

get_request_data() -> {'utils.getServerTime', #{}}.

get_fail_request_data() -> {'error.unknowMethod', #{unknow_param => vain_value}}.
