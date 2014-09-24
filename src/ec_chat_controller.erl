%% @author yuanbo.edward
%% @doc @todo Add description to ec_chat_controller.


-module(ec_chat_controller).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

-define(SERVER, ?MODULE).

start() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
init([]) ->
	usrtab = ets:new(users, [set]),
	{ok, {usrtab, users}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>


%% ====================================================================
handle_call({connect, Nick, Socket}, From, {usrtab, {usrtab, users}}) ->
	case ets:member(usrtab, Nick) of
		true ->
			new_users = users,
			Reply = nick_in_use;
		false ->
			ets:insert(usrtab, {Nick, Socket}),
			new_users = users,
			Reply = ok
	end,
    {reply, Reply, new_users};

handle_call({disconnect, Nick, Socket}, From, {usrtab, users}) ->
	case ets:member(usrtab, Nick) of
		true ->
			ets:delete(usrtab, Nick),
			new_users = users;
		false ->
			new_users = users,
			user_not_exist
	end,
	Reply = ok,
	{reply, Reply, new_users}.			


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({say, Nick, Content}, {usrtab, users}) ->
	broadcast(Nick, Content, users),
    {noreply, users};

handle_cast({join, Nick}, {usrtab, users}) ->
	broadcast(Nick, Nick ++ " has Joined", users),
	{noreply, users};

handle_cast({left, Nick}, {usrtab, users}) ->
	broadcast(Nick, Nick ++ " has Left", users),
	{noreply, users}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
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
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


broadcast(Nick, Msg, {usrtab, users}) ->
	UserList = ets:tab2list(usrtab),
	Sockets = lists:map(fun({_, Value}) -> Value end, UserList),
	lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, UserList),
	ok.

