-module(main_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RECONNECT_TIME, 60000).		% 1 min

-record(state, {id, sock_pid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ID, SockPID) ->
	gen_server:start_link(?MODULE, [ID, SockPID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ID, SockPID]) ->
   	process_flag(trap_exit, true),
	link(SockPID),
	main_mgr:register(ID, self()),
	main_internal:init(ID),
	{ok, #state{id = ID, sock_pid = SockPID}}. 

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

handle_cast({cmd, {Cmd, DataIn}}, #state{sock_pid = SockPID} = State) ->
	case main_cmd:handle(Cmd, DataIn) of
		noreply ->
			ok;
		{ok, DataOut} ->
			main_internal:send_data(SockPID, Cmd, DataOut);
		{error, ErrorCode} ->
			main_internal:send_error(SockPID, Cmd, ErrorCode)
	end,	
	{noreply, State};

handle_cast({response, Bin}, #state{sock_pid = SockPID} = State) ->
	tcp_response:send(SockPID, Bin),
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
	{stop, kill, State};

handle_info({'EXIT', _PID, _Reason}, State) ->
    erlang:send_after(?RECONNECT_TIME, self(), kill),
    {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{id = ID}) ->
	main_mgr:unregister(ID),
	main_internal:terminate(ID),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


