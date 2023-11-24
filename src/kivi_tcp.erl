%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 24.11.2023
%%
%% @doc kivi simple key-value database - tcp layer module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_tcp).

-export([start_link/0]).

-define(DEFAULT_PORT, 8081).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    case gen_tcp:listen(?DEFAULT_PORT, [binary, {active, false}]) of
        {ok, ListenSock} ->
            Pid =
                spawn_link(fun() -> 
                LogMessage = io_lib:format("TCP server starting on port: ~p.", [?DEFAULT_PORT]),
                kivi_logger:log(info, LogMessage),
                register(tcp_process, self()),
                accept_connections(ListenSock)
            end),
            {ok, Pid};

        {error, Reason} ->
            LogMessage = io_lib:format("Server error: ~p", [Reason]),
            kivi_logger:log(error, LogMessage)
    end.

accept_connections(ListenSocket) ->
    kivi_logger:log(info, "Waiting for connections..."),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            kivi_logger:log(info, "Connection accepted"),
            loop(Socket),            
            accept_connections(ListenSocket);

        {error, Reason} ->
            LogMessage = io_lib:format("Error accepting connection: ~p", [Reason]),
            kivi_logger:log(error, LogMessage),
            kivi_logger:log(error, "Listening socket closed. Server shutting down.")
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} ->
            Request = decode_request(Bin),
            LogMessage = io_lib:format("Handling request: ~s", [Request]),
            kivi_logger:log(info, LogMessage),
            handle_request(Socket, Request),            
            loop(Socket);

        {tcp_closed, Socket} ->
            LogMessage = io_lib:format("Connection closed: ~p", [Socket]),
            kivi_logger:log(error, LogMessage),
            ok;
%% split this for many response types 
        {kivi_server_response, Ref, Response} ->
            kivi_logger:log(info, "Received response from the server"),
            % Now, send the response back to the client
            gen_tcp:send(Socket, encode_response(Response)),
            loop(Socket);

        {timeout, Ref} ->
            kivi_logger:log(warn, "Timeout waiting for response from the server"),
            % Handle timeout, e.g., send an error response to the client
            gen_tcp:send(Socket, encode_response({error, "Timeout waiting for response"})),
            loop(Socket);
        
        _ ->
            io:format("Error reading from socket"),
            loop(Socket)
    end.

handle_request(Socket, Request) ->
    kivi_logger:log(info, "Sending client's request to the server"),
    Ref = make_ref(),
    gen_server:cast({global, kivi_server}, {request, self(), Ref, Request}),
    receive 
        {kivi_server_response, Ref, Response} ->
            kivi_logger:log(info, "Received response from server"),
            gen_tcp:send(Socket, encode_response(Response);

        {timeout, Ref} ->
            kivi_logger:log(warn, "Timeout waiting for response from server"),
            % add handler
            gen_tcp:send(Socket, encode_response({error, "Timeout"}))
    end.

handle_request(Socket, Request) ->
    kivi_logger:log(info, "Sending client's request to the server"),
    Ref = make_ref(),
    case Request of
        {add, Key, Value} ->
            gen_server:cast({global, kivi_server}, {add, Key, Value, Ref}),
            handle_response(Socket, Ref);

        {update, Key, Value} ->
            gen_server:cast({global, kivi_server}, {update, Key, Value, Ref}),
            handle_response(Socket, Ref);

        {get, Key} ->
            gen_server:call({global, kivi_server}, {get, Key, Ref}),
            handle_response(Socket, Ref);

        {get_all} ->
            gen_server:call({global, kivi_server}, {get_all, Ref}),
            handle_response(Socket, Ref);

        {delete, Key} ->
            gen_server:cast({global, kivi_server}, {delete, Key, Ref}),
            handle_response(Socket, Ref);

        {delete_all} ->
            gen_server:cast({global, kivi_server}, {delete_all, Ref}),
            handle_response(Socket, Ref);

        {get_size} ->
            gen_server:call({global, kivi_server}, {get_size, Ref}),
            handle_response(Socket, Ref);

        {sort, SortingBy} ->
            gen_server:call({global, kivi_server}, {sort, SortingBy, Ref}),
            handle_response(Socket, Ref);

         _ ->
            kivi_logger:log(error, "Invalid request format")
    end.

handle_response(Socket, Ref) ->
    receive 
        {kivi_server_response, Ref, Response} ->
            kivi_logger:log(info, "Received response from server"),
            gen_tcp:send(Socket, encode_response(Response));

        {timeout, Ref} ->
            kivi_logger:log(warn, "Timeout waiting for response from server"),
            % add handler
            gen_tcp:send(Socket, encode_response({error, "Timeout"}))
    end.


handle_request(Socket, {add, Key, Value}) ->  
    kivi_logger:log(info, "Sending client's request: ADD to the server"),  
    gen_server:cast({global, kivi_server}, {add, Key, Value}),
    gen_tcp:send(Socket, encode_request({add, Key, Value}));

handle_request(Socket, {update, Key, Value}) ->    
    kivi_logger:log(info, "Sending client's request: UPDATE to the server"),
    gen_server:cast({global, kivi_server}, {update, Key, Value}),
    gen_tcp:send(Socket, encode_request({update, Key, Value}));

handle_request(Socket, {get, Key}) ->
    kivi_logger:log(info, "Sending client's request: GET to the server"),    
    gen_server:call({global, kivi_server}, {get, Key}),
    gen_tcp:send(Socket, encode_request({get, Key}));

handle_request(Socket, {get_all}) ->
    kivi_logger:log(info, "Sending client's request: GET_ALL to the server"),    
    gen_server:call({global, kivi_server}, {get_all}),
    gen_tcp:send(Socket, encode_request({get_all}));

handle_request(Socket, {delete, Key}) ->
    kivi_logger:log(info, "Sending client's request: DELETE to the server"),    
    gen_server:cast({global, kivi_server}, {delete, Key}),
    gen_tcp:send(Socket, encode_request({delete, Key}));

handle_request(Socket, {delete_all}) ->
    kivi_logger:log(info, "Sending client's request: DELETE_ALL to the server"),    
    gen_server:cast({global, kivi_server}, {delete_all}),
    gen_tcp:send(Socket, encode_request({delete_all}));

handle_request(Socket, {get_size}) ->
    kivi_logger:log(info, "Sending client's request: GET_SIZE to the server"),    
    gen_server:call({global, kivi_server}, {get_size}),
    gen_tcp:send(Socket, encode_request({get_size}));

handle_request(Socket, {sort, SortingBy}) ->
    kivi_logger:log(info, "Sending client's request: SORT to the server"),    
    gen_server:call({global, kivi_server}, {sort, SortingBy}),
    gen_tcp:send(Socket, encode_request({sort, SortingBy}));