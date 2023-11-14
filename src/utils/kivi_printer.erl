%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 14.11.2023
%%
%% @doc kivi simple key-value database - response printer
%% @end
%%%-------------------------------------------------------------------

-module(kivi_printer).

-include("data.hrl").

-export([print/1,
        print_size/1,
        print_entry/2,
        print_message/1
        ]).

-spec print(map() | list()) -> string().

print(Entries) when is_list(Entries) ->
    print_list(Entries);
print(Entries) when is_map(Entries) ->
    print_list(maps:to_list(Entries)).

-spec print_list(list()) -> string().
print_list(Entries) -> 
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
    case length(Entries) of
        0 -> print_empty();
        _ -> 
            ListMapped = lists:map(fun({Key, Data}) -> format_entry(Key, Data) ++ ",\n" end, Entries),
            FormattedMap = io_lib:format("{\n~s}", [ListMapped]),
            io:format("~s\n", [FormattedMap])
    end. 

-spec print_empty() -> string().
print_empty() -> 
    io:format("Database is empty.\n").

-spec format_entry(string(), #data{}) -> string().
format_entry(Key, #data{id = Id, value = Value, updated = Timestamp}) ->
    %
    % Key => {
    %        Id: 1234,
    %        Value: "Value",
    %        Updated: "2022-22-10T10:10:10"
    %        }    
    %
    Updated = kivi_parsers:parse_timestamp_to_string(Timestamp),
    io_lib:format("\"~s\" => {\n    \"id\": ~s,\n    \"value\": ~s,\n    \"updated\": ~s\n    }", [Key, Id, Value, Updated]).

-spec print_entry(string(), #data{}) -> string().
print_entry(Key, #data{id = Id, value = Value, updated = Timestamp}) ->
    %
    % Key => {
    %        Id: 1234,
    %        Value: "Value",
    %        Updated: "2022-22-10T10:10:10"
    %        }    
    %
    FormattedEntry = format_entry(Key, #data{id = Id, value = Value, updated = Timestamp}),
    io:format("~s\n", [FormattedEntry]).

-spec print_size(integer()) -> string().
print_size(Size) ->
    Response = io_lib:format("Number of keys in database is: ~p", [Size]),
    io:format("~s\n", [Response]).

-spec print_message(string()) -> string().
print_message(Msg) ->
    io:format("~s\n", [Msg]).