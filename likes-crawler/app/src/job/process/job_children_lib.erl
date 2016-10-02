-module(job_children_lib).

%%% api
-export([create/1, remove/2, is_empty/1]).

%%%===================================================================
%%% api
%%%===================================================================

create(ChildJobRefItems) -> ordsets:from_list(ChildJobRefItems).

remove(ChildJobRef, JobChildrenList) -> ordsets:del_element(ChildJobRef, JobChildrenList).

is_empty(JobChildrenList) -> ordsets:size(JobChildrenList) =:= 0.
