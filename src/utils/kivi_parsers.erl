%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 29.10.2023
%%
%% @doc kivi simple key-value database - parsers
%% @end
%%%-------------------------------------------------------------------

-module(kivi_parsers).

-export([parse_datetime/1,
         parse_to_datetime/1,
         parse_date_log/1
        ]).

-type date() :: tuple().
-type time() :: tuple().

-spec parse_datetime(erlang:datetime()) -> string().
parse_datetime({{Year, Month, Day}, {Hours, Minutes, Seconds}}) -> 
    Datetime = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    io_lib:format(Datetime, [Year, Month, Day, Hours, Minutes, Seconds]).   

-spec parse_date_log(erlang:datetime()) -> string().
parse_date_log({{Year, Month, Day}, {Hours, Minutes, Seconds}}) -> 
    Datetime = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B",
    io_lib:format(Datetime, [Year, Month, Day, Hours, Minutes, Seconds]).

-spec parse_to_datetime(string()) -> erlang:datetime().
parse_to_datetime(Binary) when is_binary(Binary) ->
    parse_to_datetime(binary_to_list(Binary));

parse_to_datetime(String) ->
    transform_datetime_string(String).

%% transform string to datetime
-spec transform_datetime_string(string()) -> {date(), time()}.
transform_datetime_string(String) ->
    [H|T] = string:split(String, "T"),
    Date = parse_date(H),
    Time = parse_time(T),
    {Date, Time}.

%% parse date to tuple
-spec parse_date(date()) -> {date()}.
parse_date(Date) ->
    SplitDate = string:split(Date, "-", all),
    ParsedDate = [list_to_integer(X) || X <- SplitDate],   
    list_to_tuple(ParsedDate).

%% parse time to tuple
-spec parse_time(time()) -> {time()}.
parse_time(Time) ->
    NewTime = string:slice(Time, 0, 8),
    SplitTime = string:split(NewTime, ":", all),
    ParsedTime = [list_to_integer(X) || X <- SplitTime],
    list_to_tuple(ParsedTime).
    