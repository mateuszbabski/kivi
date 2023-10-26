%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 26.10.2023
%%
%% @doc kivi simple key-value database - parsers
%% @end
%%%-------------------------------------------------------------------

-module(kivi_parsers).

-export([parse_datetime/1,
         parse_to_datetime/1]).

-spec parse_datetime(erlang:datetime()) -> binary().
parse_datetime({{Year, Month, Day}, {Hours, Minutes, Seconds}}) -> 
    Datetime = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    Formatted = io_lib:format(Datetime, [Year, Month, Day, Hours, Minutes, Seconds]),
    list_to_binary(Formatted).

-spec parse_to_datetime(string()) -> erlang:datetime().
parse_to_datetime(Binary) when is_binary(Binary) ->
    parse_to_datetime(binary_to_list(Binary));

parse_to_datetime(String) ->
    transform_datetime_string(String).

%% transform string to datetime
transform_datetime_string(String) ->
    [{X} || X <- String].
