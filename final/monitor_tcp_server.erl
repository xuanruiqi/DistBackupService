-module(monitor_tcp_server).
-behavior(gen_server).

-import(file_proc, [parse_packet/1]).
-import(database, [lookup_peers/2]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

start_link(Socket) ->

	% server name is ?MODULE
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->

	%% Start accepting requests
	%% We must cast this to the worker's process, as it blocks it.
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket}}.

stop() ->
	gen_server:cast(?MODULE, stop).

handle_cast(accept, State = #state{socket=ListenSocket}) ->

	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	erlang:display("connection received!"),
	%% Boot a new listener to replace this one.
	monitor_tcp_sup:start_socket(),
	{noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
	{noreply, State}.

handle_info({tcp, Socket, <<"quit", _/binary>>}, State) ->
	gen_tcp:close(Socket),
	{stop, normal, State};
%% test
handle_info({tcp, Socket, <<"hello", _/binary>>}, State) ->
	%% send back a greeting message
	send(Socket, "why hello there!", []),
	{noreply, State};
%% test
%handle_info({tcp, Socket, Packet}, State) ->
 	%% echo message
% 	decode_packet(Packet),
% 	{noreply, State};
handle_info({tcp, Socket, Packet}, State) ->
 	erlang:display("received packet from client"),
 	erlang:display(binary_to_term(Packet)),
 	open_packet(Socket, binary_to_term(Packet)),
 	{noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
	io:fwrite("unexpected: ~p~n", [E]),
	{noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

open_packet(Socket, {join, ClientNode, ClientServPid, ClientIP, ClientPort}) ->

	erlang:display("client wants to join!"),
	MonitorListener = whereis(listener),

	MonitorListener ! {connect, ClientNode, ClientServPid, ClientIP, ClientPort};
open_packet(Socket, {logout, ClientNode, ClientServPid}) ->

	erlang:display("client wants to logout!"),
	MonitorListener = whereis(listener),

	MonitorListener ! {logout, ClientNode, ClientServPid};
open_packet(Socket, {upload, ClientNode, ClientServPid, Hash, ClientIP}) ->

	erlang:display("client wants init upload!"),

	% to return list of tuples containing 
	% every client's IP address and port number
	% each element of the list  should look like: {IP_address, Port}
	% Note: ClientNode's IP and Port should be excluded from this list

	Peers = lookup_peers(ClientNode, ClientIP),

	% send Peers back to client
	gen_tcp:send(Socket, term_to_binary(Peers));
open_packet(Socket, {download, ClientNode, ClientServPid, Hash, ClientIP}) ->

	erlang:display("client wants init download!"),

	Peers = lookup_peers(ClientNode, ClientIP),

	% send Peers back to client
	gen_tcp:send(Socket, term_to_binary(Peers)).


decode_packet(Packet) -> 
	io:fwrite("Decoding packet~n", []),
	{Filename, Hash, Content} = parse_packet(Packet),
	case RecvHash = crypto:hash(md5, Content) of
		Hash -> io:fwrite("MD5 integrity check passed~n", []),
		        file:write_file(Filename, Content);
		_    -> io:fwrite("MD5 integrity check failed~n", []),
				file:write_file("dbsdump", Content),
				io:fwrite("Received: ~s~n", [RecvHash]),
				io:fwrite("Sent: ~s~n", [Hash])
	end.
	

%% Send a message back to the client
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, true}]),
  ok.
