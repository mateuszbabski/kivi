%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 26.10.2023
%%
%% @doc kivi simple key-value database - logger
%% @end
%%%-------------------------------------------------------------------

-module(kivi_logger).

-export([]).

%% API
%% logger prints message on screen and saves it to dedicated file
info(Msg) ->    
    %% logs to screen
    CurrentTime = kivi_datetime:get_current_datetime(),
    io:format("[INFO] ~p: ~p",[CurrentTime], [Msg]).
    %% saves to file
    %error_logger:info_msg(Msg).

warn(Msg) ->    
    %% logs to screen
    CurrentTime = kivi_datetime:get_current_datetime(),
    io:format("[WARN] ~p: ~p",[CurrentTime], [Msg]).

error(Msg) ->    
    %% logs to screen
    CurrentTime = kivi_datetime:get_current_datetime(),
    io:format("[ERROR] ~p: ~p",[CurrentTime], [Msg]).


