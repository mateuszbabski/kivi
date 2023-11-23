%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 23.11.2023
%%
%% @doc kivi simple key-value database - tcp layer module
%% @end
%%%-------------------------------------------------------------------
%%%



%%%%%%%%%%%%%%%%%%% NEED INVESTIGATION HOW TO WORK WITH TCP %%%%%%%%%%%%%%%%%%%
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

        _ ->
            io:format("Error reading from socket"),
            loop(Socket)
    end.

handle_request(Socket, {add, Key, Value}) ->
    %% process that way all requests possible
    %% check how to send message to server and process response
    gen_server:cast({global, kivi_server}, {add, Key, Value}),
    gen_tcp:send(Socket, encode_request({add, Key, Value}));

encode_request(Request) ->
    term_to_binary(Request).

decode_request(Request) ->
    binary_to_term(Request).