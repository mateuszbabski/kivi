%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 27.11.2023
%%
%% @doc kivi simple key-value database - server side module
%% @end
%%%-------------------------------------------------------------------

-module(kivi_server).

-include("data.hrl").

-behaviour(gen_server).

-export([start_link/0        
        ]).

-export([init/1, 
        terminate/2, 
        handle_info/2, 
        handle_cast/2, 
        handle_call/3, 
        code_change/3
        ]).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Starts server module.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    kivi_logger:log(info, "Starting database server.."), 
    {ok, Pid} = gen_server:start_link({global, ?MODULE}, ?MODULE, [], []),
    LogMessage = io_lib:format("Database server PID: ~p", [Pid]),
    kivi_logger:log(info, LogMessage),

    register(dbserver, Pid),
    {ok, Pid}.

%%=============================================
%%                  Callbacks
%%=============================================

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Initializing empty map 
%%% as database structure.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init([]) -> {ok, map()}.
init([]) ->
    {ok, #{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_cast({atom(), string(), string()}, map()) -> {noreply, map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({add, Key, Value}, State) ->
    TcpPid = whereis(tcp_process),

    case maps:is_key(Key, State) of
        true ->
            LogMessage = io_lib:format("Server message: Adding stopped - Key: ~s already exists in database", [Key]),
            kivi_logger:log(error, LogMessage),
            TcpPid ! {kivi_server_response, {add, error}},
            {noreply, State};

        _ ->
            NewEntry = #data{id = create_id(), value = Value, updated = kivi_datetime:get_timestamp()},
            NewState = maps:put(Key, NewEntry, State),
            LogMessage = io_lib:format("Server message: Added Key: ~s to database", [Key]),
            kivi_logger:log(info, LogMessage),
            TcpPid ! {kivi_server_response, {add, ok, Key, NewEntry}},
            {noreply, NewState}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_cast({atom(), string(), string()}, map()) -> {noreply, map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({update, Key, Value}, State) ->
    TcpPid = whereis(tcp_process),

    case maps:find(Key, State) of
        {ok, Entry} ->
            UpdatedEntry = Entry#data{value = Value, updated = kivi_datetime:get_timestamp()},
            NewState = maps:put(Key, UpdatedEntry, State),
            LogMessage = io_lib:format("Server message: Updated Key: ~s to database", [Key]),
            kivi_logger:log(info, LogMessage),
            TcpPid ! {kivi_server_response, {update, ok, Key, UpdatedEntry}},
            {noreply, NewState};

        _ ->
            LogMessage = io_lib:format("Server message: Updating stopped - Key: ~s doesn't exist", [Key]),
            kivi_logger:log(error, LogMessage),
            TcpPid ! {kivi_server_response, {update, error}},
            {noreply, State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_cast({atom(), string()}, map()) -> {noreply, map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({delete, Key}, State) ->
    TcpPid = whereis(tcp_process),    
    LogMessage = io_lib:format("Server message: Deleting Key: ~s from database", [Key]),
    kivi_logger:log(info, LogMessage),

    NewState = maps:remove(Key, State),
    TcpPid ! {kivi_server_response, {delete, ok, Key}},
    {noreply, NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_cast({atom()}, map()) -> {noreply, map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({delete_all}, _State) ->
    TcpPid = whereis(tcp_process),
    kivi_logger:log(info, "Server message: Deleting all keys Client database"),

    NewState = #{},
    TcpPid ! {kivi_server_response, {delete_all, ok}},
    {noreply, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_call({atom()}, string()) -> {reply, term(), term()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({sort, SortingBy}, _From, State) ->
    TcpPid = whereis(tcp_process),
    LogMessage = io_lib:format("Server message: Sorted by ~s", [SortingBy]),
    kivi_logger:log(info, LogMessage),

    SortedData = kivi_sorter:sort_data(State, SortingBy),
    TcpPid ! {kivi_server_response, {sort, ok, SortedData}},
    {reply, SortedData, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_call({atom()}, any(), map()) -> {reply, integer(), map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({get_size}, _From, State) ->
    TcpPid = whereis(tcp_process),
    kivi_logger:log(info, "Server message: Trying to get size of database"),

    Size = maps:size(State),
    TcpPid ! {kivi_server_response, {get_size, ok, Size}},
    {reply, Size, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_call({atom(), string()}, any(), map()) -> {reply, term(), map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({get, Key}, _From, State) ->
    TcpPid = whereis(tcp_process),
    LogMessage = io_lib:format("Server message: Trying to get key: ~s from database", [Key]),
    kivi_logger:log(info, LogMessage),

    case maps:find(Key, State) of
        {ok, Entry} -> 
            TcpPid ! {kivi_server_response, {get, ok, Key, Entry}},
            {reply, Entry, State};

        error -> 
            kivi_logger:log(info, "Server message: Key not found"),
            TcpPid ! {kivi_server_response, {get, error}},
            {reply, <<"not found">>, State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% -spec handle_call({atom()}, any(), map()) -> {reply, term(), map()}.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({get_all}, _From, State) ->
    TcpPid = whereis(tcp_process),
    kivi_logger:log(info, "Server message: Trying to get all keys from database"),

    TcpPid ! {kivi_server_response, {get_all, ok, State}},
    {reply, State, State}.

-spec handle_info(string(), map()) -> {noreply, term()}.
handle_info(Msg, State) ->
    LogMessage = io_lib:format("Unknown message: ~p~n", [Msg]),
    kivi_logger:log(warn, LogMessage),
    io:format("Unknown message: ~p~n", [Msg]),
    {noreply, State}.

-spec code_change(any(), term(), any()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(string(), term()) -> ok.
terminate(Reason, _State) ->
    kivi_logger:log(error, io_lib:format("Server message: Terminated with reason: ~p", [Reason])),
    ok.

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Entry ID creator.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_id() -> string().
create_id() ->
    Id = float_to_list(rand:uniform()),
    string:slice(Id, 2, 18).
    %list_to_integer(string:slice(Id, 2, 18).
