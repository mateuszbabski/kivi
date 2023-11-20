%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 20.11.2023
%%
%% @doc kivi tcp module supervisor
%% @end
%%%-------------------------------------------------------------------

-module(kivi_tcp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [
        #{id => tcp,
          start => {kivi_tcp, start_link, []},
          restart => permanent,
          shutdown => 50000,
          type => worker}     
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
