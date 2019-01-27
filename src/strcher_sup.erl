%%%-------------------------------------------------------------------
%%% @author Ben Crabbe <bcrabbe@Bens-MBP>
%%% @copyright (C) 2019, Ben Crabbe
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2019 by Ben Crabbe <bcrabbe@Bens-MBP>
%%%-------------------------------------------------------------------
-module(strcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    TrieManager = #{id => strcher_trie_manager,
                    start => {strcher_trie_manager, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [strcher_trie_manager]},
    {ok, {SupFlags, [TrieManager]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
