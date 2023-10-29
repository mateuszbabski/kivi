%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 29.10.2023
%%
%% @doc kivi simple key-value database - date parsers test
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_parsers_test).

-include_lib("eunit/include/eunit.hrl").

% parse datetime tests
parse_datetime_test() ->
    ?assertEqual("2000-01-01T10:00:00Z", kivi_parsers:parse_datetime({{2000,01,01},{10,00,00}})),
    ?assertEqual("2020-12-24T20:10:05Z", kivi_parsers:parse_datetime({{2020,12,24},{20,10,05}})).

% parse to date log tests
parse_date_log_test() ->
    ?assertEqual("20000101T100000", kivi_parsers:parse_date_log({{2000,01,01},{10,00,00}})),
    ?assertEqual("20201224T201005", kivi_parsers:parse_date_log({{2020,12,24},{20,10,05}})).

% parse to datetime tests
parse_to_datetime_test() ->
    ?assertEqual({{2020,10,10}, {22,0,0}}, kivi_parsers:parse_to_datetime("2020-10-10T22:00:00Z")),
    ?assertEqual({{2020,10,10}, {22,0,0}}, kivi_parsers:parse_to_datetime("2020-10-10T22:00:00+100")),
    ?assertEqual({{2020,10,10}, {22,0,0}}, kivi_parsers:parse_to_datetime(<<"2020-10-10T22:00:00Z">>)).

% parse timestamp to string
parse_timestamp_to_string_test() ->
    ?assertEqual("2023-10-29T07:35:57Z", kivi_parsers:parse_timestamp_to_string({1698,564957,535552})).