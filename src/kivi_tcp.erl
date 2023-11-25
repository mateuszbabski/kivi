%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 25.11.2023
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
            accept_connections(ListenSocket); % to delete probably

        {error, Reason} ->
            LogMessage = io_lib:format("Error accepting connection: ~p", [Reason]),
            kivi_logger:log(error, LogMessage),
            kivi_logger:log(error, "Listening socket closed. Server shutting down.")
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Bin} ->
            Request = kivi_parsers:decode_request(Bin),
            LogMessage = io_lib:format("Handling request: ~p", [Request]),
            kivi_logger:log(info, LogMessage),
            handle_request(Socket, Request),            
            loop(Socket);

        {tcp_closed, Socket} ->
            LogMessage = io_lib:format("Connection closed: ~p", [Socket]),
            kivi_logger:log(error, LogMessage),
            ok;

        {kivi_server_response, Response} ->
            kivi_logger:log(info, "Received response from the server"),
            % Now, send the response back to the client
            gen_tcp:send(Socket, Response),
            loop(Socket);

        {timeout} ->
            kivi_logger:log(warn, "Timeout waiting for response from the server"),
            % Handle timeout, e.g., send an error response to the client
            gen_tcp:send(Socket, kivi_parsers:encode_request({error, "Timeout waiting for response"})),
            loop(Socket);
        
        _ ->
            io:format("Error reading from socket"),
            loop(Socket)
    end.

handle_request(Socket, Request) ->
    kivi_logger:log(info, "Sending client's request to the server"),
    case Request of
        {add, Key, Value} ->
            gen_server:cast({global, kivi_server}, {add, Key, Value}),
            handle_response(Socket);

        {update, Key, Value} ->
            gen_server:cast({global, kivi_server}, {update, Key, Value}),
            handle_response(Socket);

        {get, Key} ->
            gen_server:call({global, kivi_server}, {get, Key}),
            handle_response(Socket);

        {get_all} ->
            gen_server:call({global, kivi_server}, {get_all}),
            handle_response(Socket);

        {delete, Key} ->
            gen_server:cast({global, kivi_server}, {delete, Key}),
            handle_response(Socket);

        {delete_all} ->
            gen_server:cast({global, kivi_server}, {delete_all}),
            handle_response(Socket);

        {get_size} ->
            gen_server:call({global, kivi_server}, {get_size}),
            handle_response(Socket);

        {sort, SortingBy} ->
            gen_server:call({global, kivi_server}, {sort, SortingBy}),
            handle_response(Socket);

         _ ->
            kivi_logger:log(error, "Invalid request format")
    end.

handle_response(Socket) ->
    receive 
        {kivi_server_response, Response} ->
            kivi_logger:log(info, "Received response from server"),
            EncodedResponse = kivi_parsers:encode_request(Response),
            gen_tcp:send(Socket, EncodedResponse);

        {timeout} ->
            kivi_logger:log(warn, "Timeout waiting for response from server"),
            gen_tcp:send(Socket, kivi_parsers:encode_request({error, "Timeout"}))
    end.