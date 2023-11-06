%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 06.11.2023
%%
%% @doc kivi simple key-value database - sorter
%% @end
%%%-------------------------------------------------------------------

-module(kivi_sorter).

-include("data.hrl").

-export([sort_data/2
        ]).

-spec sort_data(map(), string()) -> list().
sort_data(DataMap, SortingBy) ->
    DataList = maps:to_list(DataMap),
    case string:to_lower(SortingBy) of
        "key" -> sort_by_key(DataList);
        "id" -> sort_by_id(DataList);
        "updated" -> sort_by_updated(DataList);
        "value" -> sort_by_value(DataList);
        _ -> DataList
    end.

sort([], _Criteria) -> [];
sort([Head|Rest], Criteria) ->
    sort([X || X <- Rest, Criteria(X) =< Criteria(Head)], Criteria) ++
    [Head] ++
    sort([X || X <- Rest, Criteria(X) > Criteria(Head)], Criteria).

criteria_by_id({_Key, #data{id = Id}}) -> Id.
criteria_by_value({_Key, #data{value = Value}}) -> Value.
criteria_by_updated({_Key,#data{updated = Updated}}) -> Updated.
criteria_by_key({Key,#data{}}) -> Key.

sort_by_key(DataList) -> sort(DataList, fun criteria_by_key/1).
sort_by_id(DataList) -> sort(DataList, fun criteria_by_id/1).
sort_by_value(DataList) -> sort(DataList, fun criteria_by_value/1).
sort_by_updated(DataList) -> sort(DataList, fun criteria_by_updated/1).