-module(tcp_client).

-export([connect/3, send/2, disconnect/1]).

connect(IP_address, Port, Options) ->

	{ok, Socket} = gen_tcp:connect(IP_address, Port, Options),
	Socket.

send(Socket, Msg) ->

	ok = gen_tcp:send(Socket, Msg).

disconnect(Socket) ->

	gen_tcp:close(Socket).