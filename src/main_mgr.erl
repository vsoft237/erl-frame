-module(main_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2, unregister/1, get_pid/1, get_all/0, get_all_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
-record(main_pid, {id, pid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(ID, PID) ->
	Object = #main_pid{id = ID, pid = PID},
	ets:insert(main_pid, Object).

unregister(ID) ->
	ets:delete(main_pid, ID).

get_pid(ID) ->
	case ets:lookup(main_pid, ID) of
		[] ->
			null;
		[Object] ->
			PID = Object#main_pid.pid,
			{ok, PID}
	end.

get_all() ->
	ets:tab2list(main_pid).

get_all_id() ->
	List = get_all(),
	[ID || #main_pid{id = ID} <- List].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	create_table(),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_table() ->
	Option = [
		public, named_table, set,
		{keypos, #main_pid.id},
		{write_concurrency, true}
	],
	ets:new(main_pid, Option). 
