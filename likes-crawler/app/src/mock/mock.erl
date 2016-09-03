-module(mock).

%%% api
-export([get_request_data/0, get_random_request_data/0]).
-export(['utils.getServerTime'/0, 'friends.get'/0, 'users.get'/0, 'photos.get'/0]).

%%%===================================================================
%%% api
%%%===================================================================

get_request_data() -> {'utils.getServerTime', #{}}.

get_random_request_data() ->
  List = [
    'utils.getServerTime',
    'friends.get',
    'users.get',
    'photos.get'
  ],

  Fun = lists:nth(rand:uniform(length(List)), List),
  erlang:apply(?MODULE, Fun, []).

'utils.getServerTime'() -> {'utils.getServerTime', #{v => '5.53'}}.

'friends.get'() -> {'friends.get', #{user_id => user(), v => '5.53'}}.

'users.get'() -> {'users.get', #{user_id => user(), v => '5.53'}}.

'photos.get'() -> {'photos.get', #{owner_id => user(), album_id => profile, v => '5.53'}}.

user() -> rand:uniform(380000000).
