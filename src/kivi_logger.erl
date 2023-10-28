%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 28.10.2023
%%
%% @doc kivi simple key-value database - logger
%% @end
%%%-------------------------------------------------------------------

-module(kivi_logger).

-record(state, {log_file = undefined}).

-export([start_logger/0,
         stop_logger/1,
         log/2
        ]).

%% API
%% logger prints message on screen and saves it to dedicated file
%% start logger - create and open file for logs
start_logger() ->
    LogFile = generate_log_file_name(),
    case file:open(LogFile, [write, append]) of
        {ok, File} ->
            LoggerState = #state{log_file = File},
            file:write(File, "Kivi Log File\n"),
            {ok, LoggerState};
        Error ->
            {error, Error}
    end.

%% stop logger - stop and close file for logs
stop_logger(#state{log_file = LogFile}) ->
    log(info, "Closing log file"),
    ok = file:close(LogFile).

%% log to the file - inputs are log status and message
log(Status, Message) ->
    case application:get_env(kivi, logger_state) of
        undefined ->
            {error, no_state};
        {ok, LoggerState} ->
            case LoggerState#state.log_file of
                undefined ->
                    {error, no_log_file};
                LogFile ->
                    LogMessage = format_log_entry(Status, Message),
                    ok = file:write(LogFile, LogMessage)
            end
    end.

generate_log_file_name() ->
    CurrentDateTime = kivi_datetime:get_current_datetime(),
    DateTimeString = kivi_parsers:parse_date_log(CurrentDateTime),
    Directory = "F:\\Logs\\",
    FileName = "Kivi" ++ DateTimeString ++ ".txt",
    Directory ++ FileName.

format_log_entry(Status, Message) ->
    CurrentTime = kivi_datetime:get_current_datetime(),
    ParsedTime = kivi_parsers:parse_datetime(CurrentTime),
    LogEntry = io_lib:format("[~s][~s]: ~s~n", [atom_to_list(Status), ParsedTime, Message]),
    LogEntry.

