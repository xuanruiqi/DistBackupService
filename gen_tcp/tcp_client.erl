-module(tcp_client).
-export([join_network/1]).
-import(naive_tcp, [start_server/1]).

join_network(Port) ->

	% start server
	{ok, Pid} = start_server(Port).

	% connect to server
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}]).
	% send msg to server
	gen_tcp:send(Socket, "hey there").



	