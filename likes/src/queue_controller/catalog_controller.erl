-module(catalog_controller).

%%% api
-export([get/2]).

%%%===================================================================
%%% api
%%%===================================================================

get(Type, Id) -> lists:map(fun term_to_binary/1, catalog_rpc:call({Type, Id})).