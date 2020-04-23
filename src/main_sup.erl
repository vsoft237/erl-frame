%%%-------------------------------------------------------------------
%% @doc main top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(main_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->	
    supervisor:start_child(?SERVER, Args).


init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},

    Server = #{
        id => main_server,
        start => {main_server, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => worker,
        modules => [main_server]
    },

    ChildSpecs = [Server],

    {ok, {SupFlags, ChildSpecs}}.
%% internal functions
