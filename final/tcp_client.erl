-module(tcp_client).

-import(file_proc, [build_packet/1, parse_packet/1]).
-export([connect/2, send/2, send_file/2, send_and_wait/2, disconnect/1]).

%%% basic funs for testing

connect(IP_address, Port) ->

	{ok, Socket} = gen_tcp:connect(IP_address, Port, [binary, {packet, 4}, {active, false}]),
	Socket.

send(Socket, Msg) ->
	ok = gen_tcp:send(Socket, Msg),
	ok.

send_file(Socket, Filename) -> 
    Packet = build_packet(Filename),
    send(Socket, Packet).

send_and_wait(Socket, Msg) ->
	ok = gen_tcp:send(Socket, Msg),
	{ok, RetVal} = gen_tcp:recv(Socket, 0),
	RetVal.

disconnect(Socket) ->

	gen_tcp:close(Socket).