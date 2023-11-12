%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 12.11.2023
%%
%% @doc kivi simple key-value database - client side module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_client).

-export([start_link/0]).

-export([add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Pid = spawn_link(fun() -> loop() end),
    register(client, Pid),
    {ok, Pid}.

loop() ->
    receive
        {add, ok, Key, NewEntry} ->
            kivi_logger:log(info, "Key added successfully."),
            kivi_printer:print_entry(Key, NewEntry),
            loop();

        {add, error} ->
            Message = "Adding key failed - Key exists in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message),
            loop();

        {update, ok, Key, UpdatedEntry} ->
            kivi_logger:log(info, "Key updated successfully."),
            kivi_printer:print_entry(Key, UpdatedEntry),
            loop();

        {update, error} ->
            Message = "Updating entry failed - Key doesn't exist in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message),
            loop();

        {get, ok, Key, Entry} ->
            kivi_logger:log(info, "Key returned successfully."),
            kivi_printer:print_entry(Key, Entry),
            loop();

        {get, error} ->
            Message = "Returning entry failed - Key doesn't exist in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message),
            loop();

        {get_all, ok, List} ->
            kivi_logger:log(info, "List returned successfully."),
            kivi_printer:print(List),
            loop();

        {delete, ok, _Key} ->
            Message = "Successfully deleted key.",
            kivi_logger:log(info, Message),
            kivi_printer:print_message(Message),
            loop();

        {delete_all, ok} ->
            Message = "Successfully deleted all keys.",
            kivi_logger:log(info, Message),
            kivi_printer:print_message(Message),
            loop();
        
        {sort, ok, SortedData} ->
            kivi_logger:log(info, "Successfully returned all sorted keys."),
            kivi_printer:print(SortedData),
            loop();

        {get_size, ok, Size} ->
            kivi_logger:log(info, "Successfully returned number of keys in database."),
            kivi_printer:print_size(Size),
            loop();

        _ ->
            Message = "Unhandled response",
            kivi_logger:log(warn, Message),
            kivi_printer:print_message(Message),
            loop()
    end.

-spec add(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
add(Key, Value) ->
    LogMessage = io_lib:format("Trying to add to Database - Key: ~s, Value: ~s", [Key, Value]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {add, Key, Value}),
    io:format("Add operation sent to server.\n").

-spec update(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
update(Key, Value) ->
    LogMessage = io_lib:format("Trying to update Key: ~s", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {update, Key, Value}),
    io:format("Update operation sent to server.\n").

-spec get(Key :: string()) -> map() | binary().
get(Key) ->
    LogMessage = io_lib:format("Trying to get ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, kivi_server}, {get, Key}),
    io:format("Get operation sent to server.\n").

-spec get_all() -> map().
get_all() ->
    kivi_logger:log(info, "Trying to get all keys from Database"),
    gen_server:call({global, kivi_server}, {get_all}),
    io:format("Get_all operation sent to server.\n").

-spec delete(Key :: string()) -> ok | {badargument, string()}.
delete(Key) ->
    LogMessage = io_lib:format("Trying to delete key ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, kivi_server}, {delete, Key}),
    io:format("Delete operation sent to server.\n").

-spec delete_all() -> ok.
delete_all() ->
    kivi_logger:log(info, "Trying to delete all keys from Database"),
    gen_server:cast({global, kivi_server}, {delete_all}),    
    io:format("Delete_all operation sent to server\n").

-spec get_size() -> integer().
get_size() ->
    kivi_logger:log(info, "Getting number of elements in database"),
    gen_server:call({global, kivi_server}, {get_size}),
    io:format("Get_size operation sent to server\n").

-spec sort(string()) -> ok.
sort(SortingBy) ->
    LogMessage = io_lib:format("Trying to sort by ~s and returning list of sorted keys from database", [SortingBy]),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, kivi_server}, {sort, SortingBy}),
    io:format("Sort operation sent to server.\n").
