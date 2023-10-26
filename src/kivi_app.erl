%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 26.10.2023
%%
%% @doc kivi simple key-value database - main app
%% @end
%%%-------------------------------------------------------------------

-module(kivi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kivi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
