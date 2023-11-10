%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 10.11.2023
%%
%% @doc kivi simple key-value database - client side module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_client).

-export([add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).
 
-spec add(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
add(Key, Value) ->
    LogMessage = io_lib:format("Trying to add to Database - Key: ~s, Value: ~s", [Key, Value]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {add, Key, Value}),
    io:format("Add operation ok\n"),
    kivi_logger:log(info, "Add operation ok").

-spec update(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
update(Key, Value) ->
    LogMessage = io_lib:format("Trying to update Key: ~s", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {update, Key, Value}),
    io:format("Update operation ok\n"),
    kivi_logger:log(info, "Update operation ok").

-spec get(Key :: string()) -> map() | binary().
get(Key) ->
    LogMessage = io_lib:format("Trying to get ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, kivi_server}, {get, Key}),
    io:format("Get operation ok\n"),
    kivi_logger:log(info, "Get operation ok").

-spec get_all() -> map().
get_all() ->
    kivi_logger:log(info, "Trying to get all keys from Database"),
    gen_server:call({global, kivi_server}, {get_all}),
    io:format("Get_all operation ok\n"),
    kivi_logger:log(info, "Get_all operation ok").

-spec delete(Key :: string()) -> ok | {badargument, string()}.
delete(Key) ->
    LogMessage = io_lib:format("Trying to delete key ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {delete, Key}),
    io:format("Delete operation ok\n"),
    kivi_logger:log(info, "Delete operation ok").

-spec delete_all() -> ok.
delete_all() ->
    kivi_logger:log(info, "Trying to delete all keys from Database"),
    gen_server:cast({global, kivi_server}, {delete_all}),
    io:format("Delete_all operation ok\n"),
    kivi_logger:log(info, "Delete_all operation ok").

-spec get_size() -> integer().
get_size() ->
    kivi_logger:log(info, "Getting number of elements in database"),
    gen_server:call({global, kivi_server}, {get_size}),
    io:format("Get_size operation ok\n"),
    kivi_logger:log(info, "Get_size operation ok").

-spec sort(string()) -> term().
sort(SortingBy) ->
    kivi_logger:log(info, "Trying to sort by ~s and returning list of sorted keys from database", [SortingBy]),
    gen_server:call({global, kivi_server}, {sort, SortingBy}),
    io:format("Sort operation ok\n"),
    kivi_logger:log(info, "Sort operation ok").
