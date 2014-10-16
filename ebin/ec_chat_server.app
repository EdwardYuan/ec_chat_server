%% application meta data, this defines the rules of starting the application

{application, ec_chat_server,
	[{description, "An chat server written in Erlang"},
	{vsn, "0.0.1"},
	{modules, [ec_application,
	           ec_sup,
	           ec_chat_svr, 
	           ec_chat_controller,
	           ec_tcp_svr]}
	 {registered, [ec_sup]},
	 {applications, [kernel, stdlib]},
	 {mod, {ec_application, []}}
	 ]}.