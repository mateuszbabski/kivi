%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 31.10.2023
%%
%% @doc kivi simple key-value database - server side module
%% @end
%%%-------------------------------------------------------------------

-module(kivi_server).

-behaviour(gen_server).

-export([
        start_link/0,
        init/0,
        add/2,
        update/2,
        get/1,
        get_all/0,
        delete/1,
        delete_all/0,
        get_size/0,
        sort/1
        ]).

-record(entry, {id, key, value, updated}).

%% start_link
start_link() ->
    kivi_logger:log(info, "Starting database server"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])

%% add_key
add(Key, Value) ->
    kivi:logger:log(info, "Trying to add to Database - Key: ~s, Value: ~s", [Key, Value]),
    gen_server:cast({local, ?MODULE}, {add, Key, Value}).

%% update_key
update(Key, Value) ->
    kivi_logger:log(info, "Trying to update Key: ~s", [Key]),
    gen_server:cast({local, ?MODULE}, {update, Key, Value}).

%% get_key
get(Key) ->
    kivi_logger:log(info, "Trying to get ~s from Database", [Key]),
    gen_server:call({local, ?MODULE}, {get, Key}).

%% get_all_keys
get_all() ->
    kivi_logger:log(info, "Trying to get all keys from Database"),
    gen_server:call({local, ?MODULE}, {get_all}).

%% delete_key
delete(Key) ->
    kivi_logger:log(info, "Trying to delete key from Database"),
    gen_server:cast({local, ?MODULE}, {delete, Key}).

%% delete_all_keys
delete_all() ->
    kivi_logger:log(info, "Trying to delete all keys from Database"),
    gen_server:cast({local, ?MODULE}, {delete_all}).

%% get_size
get_size() ->
    kivi_logger:log(info, "Getting number of elements in database"),
    gen_server:call({local, ?MODULE}, {get_size}).

%% sort
sort(String) ->
    case string:to_lower(String) of
        "id" ->
            kivi_logger:log(info, "Sorting by id and returning list of keys from database"),
            gen_server:call({local, ?MODULE}, {sort, Id});
        "key" ->
            kivi_logger:log(info, "Sorting by key and returning list of keys from database"),
            gen_server:call({local, ?MODULE}, {sort, Key});
        "updated" ->
            kivi_logger:log(info, "Sorting by updated time and returning list of keys from database"),
            gen_server:call({local, ?MODULE}, {sort, Updated});
        _ ->
            kivi_logger:log(error, "Invalid sorting value - returning list of keys without sorting"),
            gen_server:call({local, ?MODULE}, {get_all})
    end.

%%=============================================
%%                  Callbacks
%%=============================================

%% init
init() ->
    {ok, #{}}.

%% handle_cast 
handle_cast({add, Key, Value}, State) ->
    case get_if_exists(Key, State) ->
        {ok, _} ->
            kivi_logger:log(error, "Adding stopped - Key: ~s already exists in database", [Key]),
            {noreply, State};
        _ ->
            Id = create_id(),
            Timestamp = kivi_datetime:get_timestamp(),
            NewState = maps:put(Id, Key, Value, Timestamp),
            {noreply, NewState}
    end.

handle_cast({update, Key, Value}, State) ->
    case get_if_exists(Key, State) of
        {ok, Entry} ->
            UpdatedEntry = Entry#entry{value = Value, updated = kivi_datetime:get_timestamp()},
            NewState = maps:put(Key, UpdatedEntry, State),
            {noreply, NewState};
        {error, not_found} ->
            kivi_logger:log(error, "Updating stopped - Key: ~s doesn't exist", [Key]),
            {noreply, State}
    end.

handle_cast({delete, Key}, State) ->
    kivi_logger:log(info, "Trying to remove key: ~s from Database", [Key]),
    NewState = maps:remove(Key, State),
    {noreply, NewState}.

handle_cast({delete_all}, State) ->
    kivi_logger:log(info, "Trying to clear database"),
    NewState = #{},
    {noreply, NewState}.

handle_call({get_size}, _From, State) ->
    kivi_logger:log(info, "Trying to get size of database"),
    Size = maps:size(State),
    {reply, Size, State}.

%% handle_call
handle_call({}, From, State) ->

%% helpers - simple number generator
create_id() ->
    Id = float_to_list(random:uniform()),
    string:slice(Id, 2, 18).

get_if_exists(Key, Map) ->
    case maps:is_key(Key, Map) of
        true ->
            {ok, Entry} = maps:get(Key, Map),
            Entry;
        _ ->
            {error, not_found}
    end.
%%