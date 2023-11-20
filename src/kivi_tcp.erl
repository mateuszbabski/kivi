%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 20.11.2023
%%
%% @doc kivi simple key-value database - tcp layer module
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_tcp).

-export([start_link/0]).

-define(DEFAULT_PORT, 9001).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    {ok, ListenSocket} = gen_tcp:listen(DEFAULT_PORT, [{active, false}]),
    LogMessage = io_lib:format("TCP server started on port: ~p~n", [DEFAULT_PORT]),
    kivi_logger:log(info, LogMessage),
    loop(ListenSocket).

loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),    
    Pid = spawn_link(fun() -> handle_client(Socket, whereis(dbserver)) end),
    register(tcp, Pid),
    loop(ListenSocket).

handle_client(Socket, ServerPid) ->
    case gen_tcp:recv(Socket, 0) ->
        {ok, Request} -> 
            LogMessage = io_lib:format("Handling request: ~p~n", [Request]),
            kivi_logger:log(info, LogMessage),
            Response = process_request(Request),
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);

        {error, Reason} ->
            LogMessage = io_lib:format("Error receiving data from client: ~p", [Reason]),
            kivi_logger:log(error, LogMessage),
            gen_tcp:close(Socket);

        _ ->
            io:format("Error reading from socket~n")
    end.

process_request(Request) ->
    ok.