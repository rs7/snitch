-module(save).

%%% api
-export([save/1]).

%%%===================================================================
%%% api
%%%===================================================================

save([Liker, OwnerId, PhotoId]) -> lager:info("id~B photo~B_~B", [Liker, OwnerId, PhotoId]).
