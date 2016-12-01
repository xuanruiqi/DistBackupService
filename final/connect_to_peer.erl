-module(connect_to_peer).

-export([upload_to_peer/2, count_down/1]).

count_down([]) -> 0;
count_down([H | T]) ->
	erlang:display(H),
	count_down(T).

upload_to_peer([], Packet) -> 0;
upload_to_peer([H | T], Packet) ->
	{IP, Port} = H,
	Socket = connect(IP, Port), 
	gen_tcp:send(Socket, Packet),
	gen_tcp:close(Socket),
	upload_to_peer(T, Packet).

connect(IP_address, Port) ->

	{ok, Socket} = gen_tcp:connect(IP_address, Port, [binary, {packet, 4}, {active, false}]),
	Socket.

