-module(tcp_client).

-export([connect/2, send/2, disconnect/1]).

connect(IP_address, Port) ->

	{ok, Socket} = gen_tcp:connect(IP_address, Port, [{active, false}]),
	Socket.

send(Socket, Msg) ->

	ok = gen_tcp:send(Socket, Msg),
	{ok, RetVal} = gen_tcp:recv(Socket, 0),
	RetVal.

disconnect(Socket) ->

	gen_tcp:close(Socket).