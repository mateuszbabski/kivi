%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 01.11.2023
%%
%% @doc kivi simple key-value database - server side module
%% @end
%%%-------------------------------------------------------------------

-module(kivi_server).

-behaviour(gen_server).

-export([start_link/0,
        add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).

-export([init/1, 
        terminate/2, 
        handle_info/2, 
        handle_cast/2, 
        handle_call/3, 
        code_change/3
        ]).

-record(entry, {id, value, updated}).

%% start_link
start_link() ->
    kivi_logger:log(info, "Starting database server"),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% add_key
add(Key, Value) ->
    LogMessage = io_lib:format("Trying to add to Database - Key: ~s, Value: ~s", [Key, Value]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, ?MODULE}, {add, Key, Value}).

%% update_key
update(Key, Value) ->
    LogMessage = io_lib:format("Trying to update Key: ~s", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, ?MODULE}, {update, Key, Value}).

%% get_key
get(Key) ->
    LogMessage = io_lib:format("Trying to get ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, ?MODULE}, {get, Key}).

%% get_all_keys
get_all() ->
    LogMessage = io:format("Trying to get all keys from Database"),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, ?MODULE}, {get_all}).

%% delete_key
delete(Key) ->
    LogMessage = io_lib:format("Trying to delete key ~s from Database", [Key]),
    kivi_logger:log(info, LogMessage),
    gen_server:cast({global, ?MODULE}, {delete, Key}).

%% delete_all_keys
delete_all() ->
    kivi_logger:log(info, "Trying to delete all keys from Database"),
    gen_server:cast({global, ?MODULE}, {delete_all}).

%% get_size
get_size() ->
    LogMessage = io:format("Getting number of elements in database~n"),
    kivi_logger:log(info, LogMessage),
    gen_server:call({global, ?MODULE}, {get_size}).

%% sort
sort(String) ->
    case string:to_lower(String) of
        "id" ->
            kivi_logger:log(info, "Sorting by id and returning list of keys from database"),
            gen_server:call({global, ?MODULE}, {sort, id});
        "key" ->
            kivi_logger:log(info, "Sorting by key and returning list of keys from database"),
            gen_server:call({global, ?MODULE}, {sort, key});
        "updated" ->
            kivi_logger:log(info, "Sorting by updated time and returning list of keys from database"),
            gen_server:call({global, ?MODULE}, {sort, updated});
        _ ->
            kivi_logger:log(error, "Invalid sorting value - returning list of keys without sorting"),
            gen_server:call({global, ?MODULE}, {get_all})
    end.

%%=============================================
%%                  Callbacks
%%=============================================

%% init
init([]) ->
    {ok, #{}}.

%% handle_cast %% WORK ON DATA STRUCTURE TO KEEP ENTITY
handle_cast({add, Key, Value}, State) ->
    io:format("ADDING ~s~n", [Key]),
    case get_if_exists(Key, State) of
        {ok, _Entry} ->
            LogMessage = io_lib:format("Adding stopped - Key: ~s already exists in database", [Key]),
            kivi_logger:log(error, LogMessage),
            {noreply, State};
        _ ->
            NewEntry = #entry{id = create_id(), value = Value, updated = kivi_datetime:get_timestamp()},
            NewState = maps:put(Key, NewEntry, State),
            LogMessage = io_lib:format("Added Key: ~s to database", [Key]),
            kivi_logger:log(info, LogMessage),
            {noreply, NewState}
    end;

handle_cast({update, Key, Value}, State) ->
    case get_if_exists(Key, State) of
        {ok, Entry} ->
            UpdatedEntry = Entry#entry{value = Value, updated = kivi_datetime:get_timestamp()},
            NewState = maps:put(Key, UpdatedEntry, State),
            LogMessage = io_lib:format("Updated Key: ~s to database", [Key]),
            kivi_logger:log(info, LogMessage),
            {noreply, NewState};
        _ ->
            LogMessage = io_lib:format("Updating stopped - Key: ~s doesn't exist", [Key]),
            kivi_logger:log(error, LogMessage),
            {noreply, State}
    end;

handle_cast({delete, Key}, State) ->
    LogMessage = io_lib:format("Deleting Key: ~s from database", [Key]),
    kivi_logger:log(info, LogMessage),
    NewState = maps:remove(Key, State),
    {noreply, NewState};

handle_cast({delete_all}, _State) ->
    kivi_logger:log(info, "Deleting all keys from database~n"),
    NewState = #{},
    {noreply, NewState}.

%% handle_call
handle_call({sort, id}, _From, State) ->
    Message = io:format("Sorted by id"),
    kivi_logger:log(info, "Sorted by id"),
    {reply, Message, State};
handle_call({sort, key}, _From, State) ->
    Message = io:format("Sorted by key"),
    kivi_logger:log(info, "Sorted by key"),
    {reply, Message, State};
handle_call({sort, updated}, _From, State) ->
    Message = io:format("Sorted by updated time"),
    kivi_logger:log(info, "Sorted by updated time"),
    {reply, Message, State};

handle_call({get_size}, _From, State) ->
    kivi_logger:log(info, "Trying to get size of database"),
    Size = maps:size(State),
    {reply, Size, State};

handle_call({get, Key}, _From, State) ->
    LogMessage = io_lib:format("Trying to get key: ~s from database", [Key]),
    kivi_logger:log(info, LogMessage),
    Size = maps:size(State),
    {reply, Size, State};

handle_call({get_all}, _From, State) ->
    kivi_logger:log(info, "Trying to get all keys from database"),
    Size = maps:size(State),
    {reply, Size, State}.

handle_info(Msg, State) ->
    LogMessage = io_lib:format("Unknown message: ~p~n", [Msg]),
    kivi_logger:log(warn, LogMessage),
    io:format("Unknown message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% helpers - simple number generator
create_id() ->
    Id = float_to_list(rand:uniform()),
    string:slice(Id, 2, 18).

get_if_exists(Key, Map) ->
    case maps:iskey(Key, Map) of
        true ->
            maps:get(Key, Map);
        _ ->
            <<"not found">>
    end.
