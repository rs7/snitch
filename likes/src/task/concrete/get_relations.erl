-module(get_relations).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(Users) ->
  Params =
    #{
      user_ids => Users,
      fields => relation,
      v => '5.60'
    },

  {'users.get', Params}.

response({response, #{<<"items">> := Items}}, _Users) ->
  [
    begin
      User = maps:get(<<"id">>, Item),

      Relation = case maps:find(<<"relation">>, Item) of
        {ok, Value} -> Value;
        error -> -1
      end,

      {save_relation, {User, Relation}}
    end
    ||
    Item <- Items
  ].
