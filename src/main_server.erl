-module(main_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RECONNECT_TIME, 60000).		% 1 min

-record(state, {id, sock_pid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ID, SockPID]) ->
   	process_flag(trap_exit, true),
	link(SockPID),
	main:register(ID, self()),
	main_interface:init(ID),
	{ok, #state{id = ID, sock_pid = SockPID}. 

%%%===================================================================
%%% call 
%%%===================================================================

handle_call({mfa, {M, F, A}}, _From, State) ->
	Reply = erlang:apply(M, F, A),
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%%===================================================================
%%% cast 
%%%===================================================================

handle_cast({cmd, {Cmd, DataIn}}, {sock_pid = SockPID} = State) ->
	case main_cmd:handle(Cmd, DataIn) of
		noreply ->
			ok;
		{ok, DataOut} ->
			response:send_data(SockPID, Cmd, DataOut);
		{error, ErrorCode} ->
			response:send_error(SockPID, Cmd, ErrorCode);
	end,	
	{noreply, State};

handle_cast({send_data, {Cmd, Data}, {sock_pid = SockPID} = State) ->
	response:send_data(SockPID, Cmd, Data);
	{noreply, State};

handle_cast({mfa, {M, F, A}}, State) ->
	erlang:apply(M, F, A),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%%===================================================================
%%% info 
%%%===================================================================

handle_info({mfa, {M, F, A}}, State) ->
	erlang:apply(M, F, A),
	{noreply, State};

handle_info(kill, State) ->
	{stop, normal, State};

handle_info({'EXIT', PID, _Reason}, State) ->
    erlang:send_after(?RECONNECT_TIME, self(), kill),
    {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, {id = ID} = State) ->
	main_interface:terminate(ID),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


