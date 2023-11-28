%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 28.11.2023
%%
%% @doc kivi simple key-value database - client side module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_client).

-export([start_link/0
        ]).

-export([add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Start clients module that spawns connection
%%% to the host.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Pid = spawn_link(fun() -> init() end),
    register(client, Pid),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Initial connection to host and loop over socket
%%% it to keep connection open.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    case gen_tcp:connect({127,0,0,1}, 8081, []) of
        {ok, Socket} ->
            loop(Socket);

        {error, Reason} ->
            LogMessage = io_lib:format("Connection error: ~p", [Reason]),
            kivi_logger:log(error, LogMessage)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Loop over connection socket.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Socket) ->
    receive
        {Request, _From} ->
            send_request(Socket, Request),
            loop(Socket);

        _ ->
            LogMessage = "Unhandled request",
            kivi_logger:log(warn, LogMessage),
            loop(Socket)

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Client side API actions.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
add(Key, Value) ->
    LogMessage = io_lib:format("Trying to add to Database - Key: ~s, Value: ~s", [Key, Value]),
    kivi_logger:log(info, LogMessage),
    Request = {add, Key, Value},
    client ! {Request, self()},    
    io:format("Add operation sent to server.\n").

-spec update(Key :: string(), Value :: string()) -> ok | {badargument, string()}.
update(Key, Value) ->
    LogMessage = io_lib:format("Trying to update Key: ~s", [Key]),
    kivi_logger:log(info, LogMessage),
    Request = {update, Key, Value},
    client ! {Request, self()},    
    io:format("Update operation sent to server.\n").

-spec get(Key :: string()) -> map() | binary().
get(Key) ->
    LogMessage = io_lib:format("Trying to get ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    Request = {get, Key},
    client ! {Request, self()},    
    io:format("Get operation sent to server.\n").

-spec get_all() -> map().
get_all() ->
    kivi_logger:log(info, "Trying to get all keys from Database"),
    Request = {get_all},
    client ! {Request, self()},    
    io:format("Get_all operation sent to server.\n").

-spec delete(Key :: string()) -> ok | {badargument, string()}.
delete(Key) ->
    LogMessage = io_lib:format("Trying to delete key ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    Request = {delete, Key},
    client ! {Request, self()},    
    io:format("Delete operation sent to server.\n").

-spec delete_all() -> ok.
delete_all() ->
    kivi_logger:log(info, "Trying to delete all keys from Database"),
    Request = {delete_all},
    client ! {Request, self()},       
    io:format("Delete_all operation sent to server\n").

-spec get_size() -> integer().
get_size() ->
    kivi_logger:log(info, "Getting number of elements in database"),
    Request = {get_size},
    client ! {Request, self()},    
    io:format("Get_size operation sent to server\n").

-spec sort(string()) -> ok.
sort(SortingBy) ->
    LogMessage = io_lib:format("Trying to sort by ~s and returning list of sorted keys from database", [SortingBy]),
    kivi_logger:log(info, LogMessage),
    Request = {sort, SortingBy},
    client ! {Request, self()},    
    io:format("Sort operation sent to server.\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding and sending request to the server
%%% through Tcp module.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_request(Socket, Request) ->
    gen_tcp:send(Socket, kivi_parsers:encode_request(Request)),
    receive
        {tcp, Socket, Bin} ->
            kivi_logger:log(info, "Received encoded response from the server."),
            handle_response(Bin);

        _ ->
            io:format("Error receiving response from server")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Handling response from the the server
%%% sent through Tcp module.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_response(binary()) -> string().
handle_response(Response) ->
   kivi_logger:log(info, "Decoding response from the server..."),
   DecodedResponse = kivi_parsers:decode_response(Response),
   case DecodedResponse of
        {add, ok, Key, NewEntry} ->
            kivi_logger:log(info, "Key added successfully."),
            kivi_printer:print_entry(Key, NewEntry);

        {add, error} ->
            Message = "Adding key failed - Key exists in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message);

        {update, ok, Key, UpdatedEntry} ->
            kivi_logger:log(info, "Key updated successfully."),
            kivi_printer:print_entry(Key, UpdatedEntry);

        {update, error} ->
            Message = "Updating entry failed - Key doesn't exist in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message);

        {get, ok, Key, Entry} ->
            kivi_logger:log(info, "Key returned successfully."),
            kivi_printer:print_entry(Key, Entry);

        {get, error} ->
            Message = "Returning entry failed - Key doesn't exist in database.",
            kivi_logger:log(error, Message),
            kivi_printer:print_message(Message);

        {get_all, ok, List} ->
            kivi_logger:log(info, "List returned successfully."),
            kivi_printer:print(List);

        {delete, ok, _Key} ->
            Message = "Successfully deleted key.",
            kivi_logger:log(info, Message),
            kivi_printer:print_message(Message);

        {delete_all, ok} ->
            Message = "Successfully deleted all keys.",
            kivi_logger:log(info, Message),
            kivi_printer:print_message(Message);
        
        {sort, ok, SortedData} ->
            kivi_logger:log(info, "Successfully returned all sorted keys."),
            kivi_printer:print(SortedData);

        {get_size, ok, Size} ->
            kivi_logger:log(info, "Successfully returned number of keys in database."),
            kivi_printer:print_size(Size);

        _ ->
            Message = "Unhandled response",
            kivi_logger:log(warn, Message),
            kivi_printer:print_message(Message)
   end.
