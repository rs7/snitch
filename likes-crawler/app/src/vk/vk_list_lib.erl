-module(vk_list_lib).

%%% api
-export([]).

%%%===================================================================
%%% api
%%%===================================================================

%%%===================================================================
%%% internal
%%%===================================================================

get_offsets(PageSize, From, To) when From > To -> [];

get_offsets(PageSize, From, To) -> [From | get_offsets(PageSize, From + PageSize, To)].

get_page_count(PageSize, ItemCount) -> util:ceil(ItemCount / PageSize).

get_page_offset(PageSize, PageNumber) -> (PageNumber - 1) * PageSize.
