%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 11.11.2023
%%
%% @doc kivi simple key-value database - response printer
%% @end
%%%-------------------------------------------------------------------

-module(kivi_printer).

-include("data.hrl").

-export([print/1,
        format_entry/2
        ]).

print(List) ->
    case lists:sum(List) of
        0 -> print_empty();
        _ -> print_list(List)
    end.

-spec print_empty() -> list().
print_empty() -> [].

% check data structures
-spec print_list(list()) -> string().
print_list(List) -> 
    %   {
    %       Key =>  {
    %               Id: 1234,
    %               Value: "Value",
    %               Updated: "2022-22-10T10:10:10"
    %               },
    %       Key2 => {
    %               Id: 12345,
    %               Value: "Value2",
    %               Updated: "2022-22-11T10:10:10"
    %               }    
    %   }
    MappedList = lists:map(fun({Key, Data}) -> format_entry(Key, Data) end, maps:to_list(List)),
    FormattedList = io_lib:format("{\n~s\n}", [lists:concat(MappedList, ", \n")]),
    FormattedList.

-spec format_entry(string(), #data{}) -> string().
format_entry(Key, Entry) ->
    %parse timestamp to normal date time
    %
    % Key => {
    %        Id: 1234,
    %        Value: "Value",
    %        Updated: "2022-22-10T10:10:10"
    %        }    
    %
    #data{id = Id, value = Value, updated = Timestamp} = Entry,
    Updated = kivi_parsers:parse_timestamp_to_string(Timestamp),    
    FormattedEntry = io_lib:format("{\n~s => {\nid: ~s,\n value: ~s,\n updated: ~s\n}", [Key, Id, Value, Updated]),
    FormattedEntry.