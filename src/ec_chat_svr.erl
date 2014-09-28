%% @author yuanbo.edward
%% @doc @todo Add description to ec_chat_svr.


-module(ec_chat_svr).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, connect/2, disconnect/2, say/3]).

start(Port) ->
	ec_chat_controller:start(),
	ec_tcp_svr:start(?MODULE, Port, {?MODULE, loop}).

%% ====================================================================
%% Internal functions
%% ====================================================================


%% main loop of the server, waiting for requests from the clients
loop(Nick, Socket) ->
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Data} ->
			io:format("Data: ~p~n", binary_to_list(Data)),
			Message = binary_to_list(Data),
			{Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
			case Command of
				"SAY" ->
					say(Nick, Socket, clean(Content));
				others ->
					{ok, others}
			end;
		{error, closed} ->
			ok
	end.

%% connect to server
connect(Nick, Socket) ->
	Response = gen_server:call(ec_chat_controller, {connect, Nick, Socket}, 5000),
	case Response of
		{ok, []} ->
			gen_tcp:send(Socket, Nick ++ " Connected"), 
			join(Nick, Socket),
			loop(Nick, Socket);
		nick_in_use ->
			gen_tcp:send(Socket, "NickName is being used.")
	end.


%% disconnect from server
disconnect(Nick, Socket) ->
	Response = gen_server:call(ec_chat_controller, {disconnect, Nick, Socket}, 5000),
	case Response of
		ok ->
			gen_tcp:send(Socket, Nick ++ " disconnected"),
			left(Nick, Socket),
			ok;
		user_not_found ->
			gen_tcp:send(Socket, "Looking for user error, disconnect"),
			ok
	end,
	%% close the socket after the client disconnected from the server
	gen_tcp:close(Socket).

%% the client has send some messages to others
say(Nick, Socket, Content) ->
	gen_server:cast(ec_chat_controller, {say, Nick, Socket, Content}),
	loop(Nick, Socket).

%% anyone who join the chat
join(Nick, Socket) ->
	gen_server:cast(ec_chat_controller, {join, Nick, Socket}).

%% anyone who left the chat
left(Nick, Socket) ->
	gen_server:cast(ec_chat_controller, {leave, Nick, Socket}).

%% clean data
clean(Data) ->
    string:strip(Data, both, $\n).

	


	







		

