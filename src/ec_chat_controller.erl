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
	ets:new(users, [set, named_table]),
	{ok, users}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% ====================================================================
handle_call({connect, Nick, Socket}, _From, users) ->
	case ets:member(users, Nick) of
		true ->
			New_Users = ets:tab2list(users),
			Reply = nick_in_use;
		false ->
			ets:insert(users, {Nick, Socket}),
			New_Users = ets:tab2list(users),
			Reply = ok
	end,
    {reply, Reply, New_Users};

handle_call({disconnect, Nick, _Socket}, _From, users) ->
	case ets:member(users, Nick) of
		true ->
			ets:delete(users, Nick),
			New_Users = ets:tab2list(users);
		false ->
			New_Users = ets:tab2list(users),
			user_not_exist
	end,
	Reply = ok,
	{reply, Reply, New_Users}.			


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% ====================================================================
handle_cast({say, Nick, Content}, users) ->
	broadcast(Nick, Content, users),
    {noreply, users};

handle_cast({join, Nick}, users) ->
	broadcast(Nick, Nick ++ " has Joined", users),
	{noreply, users};

handle_cast({left, Nick}, users) ->
	broadcast(Nick, Nick ++ " has Left", users),
	{noreply, users}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
%% ====================================================================
terminate(_Reason, State) ->
    {ok, State}.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


broadcast(_Nick, Msg, users) ->
	UserList = ets:tab2list(users),
	Sockets = lists:map(fun({_, Value}) -> Value end, UserList),
	lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets),
	ok.

