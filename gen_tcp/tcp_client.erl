-module(tcp_client).
-export([join_network/1]).
-import(naive_tcp, [start_server/1]).

% give this 8099 ALWAYS
join_network(Port) ->

	SomeHostInNet = "localhost",

	erlang:display("starting server.."),

	% start server
	{ok, Pid} = start_server(Port),

	erlang:display("connecting to server.."),

	% connect to server
	{ok, Socket} = gen_tcp:connect(SomeHostInNet, Port, [binary, {active, true}]),

	% send msg to server
	ok = gen_tcp:send(Socket, "hey there"),
	ok = gen_tcp:close(Socket).



	