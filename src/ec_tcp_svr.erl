%% @author yuanbo.edward
%% @doc @todo Add description to ec_tcp_svr.


-module(ec_tcp_svr).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3]).


-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-record(svr_state, {port, loop, ip=any, lsock=null}).


start(Name, Port, Loop) ->
	State = #svr_state{port=Port, loop=Loop},
	gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>

init(State = #svr_state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, LSocket} ->
			NewState = State#svr_state{lsock = LSocket},
			{ok, accept(NewState)};
		{error, Reason} ->
			{stop, Reason}
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>

handle_cast({accepted, _Pid}, State=#svr_state{}) ->
	{noreply, accept(State)}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>

handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, State) ->
    {ok, State}.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

accept_loop({Server, LSocket, {Module, LoopFunction}}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(Server, {accepted, self()}),
	Module:LoopFunction(Socket).

accept(State=#svr_state{lsock=LSocket, loop=Loop}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
	State.










