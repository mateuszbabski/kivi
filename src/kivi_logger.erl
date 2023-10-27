%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 27.10.2023
%%
%% @doc kivi simple key-value database - logger
%% @end
%%%-------------------------------------------------------------------

-module(kivi_logger).

-export([start/1,
         stop/1,
         info/1,
         info/2,
         warn/1,
         warn/2,
         error/1,
         error/2]).

%% API
%% logger prints message on screen and saves it to dedicated file
%% start logger - create and open file for logs
start(FilePath) ->
    {ok, File} = file:open(FilePath, [write, append]),
    {ok, #state{file=File}}.

%% stop logger - stop and close file for logs
stop(State) ->
    file:close(State#state.file).

info(State, Msg) -> 
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),    
    Message = io:format("[INFO][~p]: ~p~n",[ParsedTime, Msg]),
    {ok, _} = file:write(State#state.file, io_lib:format("~s~n", [Message])),
    State.

warn(State, Msg) -> 
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),
    Message = io:format("[WARN][~p]: ~p~n",[ParsedTime, Msg]),
    {ok, _} = file:write(State#state.file, io_lib:format("~s~n", [Message])),
    State.

error(State, Msg) -> 
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),
    Message = io:format("[ERROR][~p]: ~p~n",[ParsedTime, Msg]),
    {ok, _} = file:write(State#state.file, io_lib:format("~s~n", [Message])),
    State.

info(Msg) ->    
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),    
    io:format("[INFO][~p]: ~p~n",[ParsedTime, Msg]).

warn(Msg) ->    
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),
    io:format("[WARN][~p]: ~p~n",[ParsedTime, Msg]).

error(Msg) ->    
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),
    io:format("[ERROR][~p]: ~p~n",[ParsedTime, Msg]).


