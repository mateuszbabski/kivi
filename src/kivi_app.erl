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
    {ok, LoggerState} = kivi_logger:start_logger(),
    application:set_env(kivi, logger_state, LoggerState),
    kivi_sup:start_link().

stop(_State) ->
    LoggerState = application:get_env(kivi, logger_state),
    kivi_logger:stop_logger(LoggerState),
    ok.

%% internal functions
