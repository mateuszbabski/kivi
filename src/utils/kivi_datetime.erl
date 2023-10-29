%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 29.10.2023
%%
%% @doc kivi simple key-value database - datetime helper
%% @end
%%%-------------------------------------------------------------------

-module(kivi_datetime).

-export([get_current_datetime/0, 
         get_timestamp/0,
         get_current_localtime/0
        ]).

-spec get_current_datetime() -> erlang:datetime().
get_current_datetime() -> 
    erlang:universaltime().

-spec get_current_localtime() -> erlang:datetime().
get_current_localtime() -> 
    erlang:localtime().

-spec get_timestamp() -> erlang:timestamp().
get_timestamp() ->
    erlang:timestamp().