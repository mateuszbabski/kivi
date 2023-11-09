%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 09.11.2023
%%
%% @doc kivi simple key-value database - client side module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_client).

-include("data.hrl").

-export([add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).


%% add specs
%% correct returns in functions
%% add error handling here and on server side

add(Key, Value) ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | ADD Key: ~s, Value: ~s", [ServerPid, Key, Value]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {add, Key, Value}},
    receive
        {ServerPid, {response, add, ok}} ->
            io:format("Add operation ok");
        %{ServerPid, {response, add, {error, Reason}}} ->
        _ ->
            io:format("Add operation failed")
    end.

update(Key, Value) ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | UPDATE Key: ~s, Value: ~s", [ServerPid, Key, Value]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {update, Key, Value}},
    receive
        {ServerPid, {response, update, ok}} ->
            io:format("Update operation ok~n");
        %{ServerPid, {response, update, {error, Reason}}} ->
        _ ->
            io:format("Update operation failed~n")
    end.

get(Key) ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | GET Key: ~s", [ServerPid, Key]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {get, Key}},
    receive
        {ServerPid, {response, get, Entry}} ->
            io:format("Get operation ok~n");
        %{ServerPid, {response, get, {error, Reason}}} ->
        _ ->
            io:format("Get operation failed~n")
    end.

get_all() ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | GET_ALL", [ServerPid]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {get_all}},
    receive
        {ServerPid, {response, get_all, List}} ->
            io:format("Get_all operation ok~n");
        %{ServerPid, {response, get_all, {error, Reason}}} ->
        _ ->
            io:format("Get_all operation failed~n")
    end.

delete(Key) ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | DELETE Key: ~s", [ServerPid, Key]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {delete, Key}},
    receive
        {ServerPid, {response, delete, ok}} ->
            io:format("Delete operation ok~n");
        %{ServerPid, {response, delete, {error, Reason}}} ->
        _ ->
            io:format("Delete operation failed~n")
    end.

delete_all() ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | DELETE_ALL", [ServerPid]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {delete_all}},
    receive
        {ServerPid, {response, delete_all, ok}} ->
            io:format("Delete_all operation ok~n");
        %{ServerPid, {response, delete_all, {error, Reason}}} ->
        _ ->
            io:format("Delete_all operation failed~n")
    end.

get_size() ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | GET_SIZE", [ServerPid]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {get_size}},
    receive
        {ServerPid, {response, get_size, Size}} ->
            io:format("Get_size operation ok~n");
        %{ServerPid, {response, get_size, {error, Reason}}} ->
        _ ->
            io:format("Get_size operation failed~n")
    end.

sort(SortingBy) ->
    ServerPid = get_server_pid(),
    LogMessage = io_lib:format("Trying to send request to server: ~p | SORT SortingBy: ~s", [ServerPid, SortingBy]),
    kivi_logger:log(info, LogMessage),
    ServerPid ! {self(), {sort, SortingBy}},
    receive
        {ServerPid, {response, sort, SortedList}} ->
            io:format("Sort operation ok~n");
        %{ServerPid, {response, sort, {error, Reason}}} ->
        _ ->
            io:format("Sort operation failed~n")
    end.

-spec get_server_pid() -> pid().
get_server_pid() ->
    whereis(dbserver).