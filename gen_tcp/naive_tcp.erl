-module(naive_tcp).
-export([start_server/1]).
 
start_server(Port) ->
	Pid = spawn_link(fun() ->
		erlang:display("spawning listener.."),
		{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
		spawn(fun() -> acceptor(ListenSocket) end),
		timer:sleep(infinity)
		end),
	{ok, Pid}.
 
acceptor(ListenSocket) ->
	erlang:display("accepting ListenSocket.."),
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> acceptor(ListenSocket) end),
	handle(Socket).
 
%% Echoing back whatever was obtained
handle(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, <<"quit", _/binary>>} ->
		gen_tcp:close(Socket);
		{tcp, Socket, Msg} ->
		gen_tcp:send(Socket, Msg),
		handle(Socket)
	end.