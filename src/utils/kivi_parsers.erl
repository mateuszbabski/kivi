%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 27.10.2023
%%
%% @doc kivi simple key-value database - parsers
%% @end
%%%-------------------------------------------------------------------

-module(kivi_parsers).

-export([parse_datetime/1,
         parse_to_datetime/1
        ]).

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
    [H|T] = string:split(String, "T"),
    Date = parse_date(H),
    Time = parse_time(T),
    {Date, Time}.

%% parse date to tuple
parse_date(Date) ->
    SplitDate = string:split(Date, "-", all),
    ParsedDate = [list_to_integer(X) || X <- SplitDate],   
    list_to_tuple(ParsedDate).

%% parse time to tuple
parse_time(Time) ->
    NewTime = string:slice(Time, 0, 8),
    SplitTime = string:split(NewTime, ":", all),
    ParsedTime = [list_to_integer(X) || X <- SplitTime],
    list_to_tuple(ParsedTime).
    