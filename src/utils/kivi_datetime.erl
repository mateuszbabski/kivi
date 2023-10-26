%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 26.10.2023
%%
%% @doc kivi simple key-value database - datetime helper
%% @end
%%%-------------------------------------------------------------------

-module(kivi_datetime).

-export([get_current_datetime/0]).

get_current_datetime() -> 
    erlang:localtime().