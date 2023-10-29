%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 29.10.2023
%%
%% @doc kivi simple key-value database - datetime helper
%% @end
%%%-------------------------------------------------------------------

-module(kivi_datetime).

-export([get_current_datetime/0]).

-spec get_current_datetime() -> erlang:datetime().
get_current_datetime() -> 
    erlang:universaltime().