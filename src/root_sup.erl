-module(root_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
		supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},

    MainSup= #{
        id => main_sup,
        start => {main_sup, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => supervisor,
        modules => [main_sup]
    },
    MainMgr= #{
        id => main_mgr,
        start => {main_mgr, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => supervisor,
        modules => [main_mgr]
    },
	
    ChildSpecs = [MainSup, MainMgr],
    {ok, {SupFlags, ChildSpecs}}.

