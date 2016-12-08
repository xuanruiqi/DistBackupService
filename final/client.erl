-module(client).

-import(file_proc, [build_packet/1, parse_packet/1, write_peer_file/2]).
-import(tcp_sup, [start_link/0, start_link/2]).
-import(database, [init_client_dets/0, add_file_to_table/1,
                   lookup_file/1]).
-export([join/3, logout/2, init_upload/3, init_download/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(MonitorIP, MonitorPort, MyPort) ->

	init_client_dets(),

	MyPid = self(),

	% start my tcp_server
	tcp_sup:start_link(MyPort, MyPid),

	% wait for tcp_sup to send back the listening port
	receive
		MyNewPort -> 
			erlang:display("received Port number from child proc!"),
			erlang:display(MyNewPort)
	end,

	% find my IP address
	MyIP = local_ip_v4(),

	% get pid of my server
	ServPid = whereis(tcp_sup),


	case connect(MonitorIP, MonitorPort) of
		{ok, Socket} ->
			% build join request packet
			Packet = term_to_binary({join, node(), pid_to_list(ServPid), MyIP, MyNewPort}),
			% send request to join
			gen_tcp:send(Socket, Packet),

			gen_tcp:close(Socket);
		{error, Reason} -> erlang:display("You cannot connect to the monitor at this time.")
	end.



logout(MonitorIP, MonitorPort) ->

	% get pid of my server
	ServPid = whereis(tcp_sup),

	case connect(MonitorIP, MonitorPort) of
		{ok, Socket} ->
			% build join request packet
			Packet = term_to_binary({logout, node(), ServPid}),
			% send request to join
			gen_tcp:send(Socket, Packet),

			gen_tcp:close(Socket);
		{error, Reason} -> erlang:display("You cannot connect to the monitor at this time.")
	end.

init_upload(MonitorIP, MonitorPort, File) ->

	% get pid of my server
	ServPid = whereis(tcp_sup),

	% find my IP address
	MyIP = local_ip_v4(),

	% build file packet
	FilePacket = build_packet(File),

	% parse file packet to get Hash
	{Filename, Hash, Content} = parse_packet(FilePacket),

	add_file_to_table({Filename, Hash}),

	% build init_upload request packet
	Packet = term_to_binary({upload, node(), ServPid, Hash, MyIP}),

	case connect(MonitorIP, MonitorPort) of
		{ok, Socket} ->
			% send request to init_upload
			gen_tcp:send(Socket, Packet),
			% catch return value
			{ok, RetVal} = gen_tcp:recv(Socket, 0),

			% convert RetVal to list of Peers
			Peers = binary_to_term(RetVal),

			gen_tcp:close(Socket), 

			erlang:display(Peers),

			case Peers of 
				[] -> 
					erlang:display("There are no peers to accept uploads at this time.");
				List -> 
					% call helper fun to upload File to every Peer in Peers
					upload(FilePacket, Peers)
			end;

			% call helper fun to upload File to every Peer in Peers
			%upload(FilePacket, Peers);
		{error, Reason} -> erlang:display("You cannot connect to the monitor at this time.")
	end.


init_download(MonitorIP, MonitorPort, File) ->
	
	% get pid of my server
	ServPid = whereis(tcp_sup),

	% find my IP address
	MyIP = local_ip_v4(),

	Hash = lookup_file(File),

	% build init_download request packet
	Packet = term_to_binary({download, node(), ServPid, Hash, MyIP}),

	case connect(MonitorIP, MonitorPort) of
		{ok, Socket} ->
			% send request to init_upload
			gen_tcp:send(Socket, Packet),
			% catch return value
			{ok, RetVal} = gen_tcp:recv(Socket, 0),

			% convert RetVal to list of Peers
			Peers = binary_to_term(RetVal),

			gen_tcp:close(Socket), 

			erlang:display(Peers),

			case Peers of 
				[] -> 
					erlang:display("There are no peers to download from at this time.");
				List -> 
					% call helper fun to download File from every Peer in Peers
					download(Hash, Peers, File)			
			end;
			% call helper fun to download File from every Peer in Peers
			%download(Hash, Peers, File);
		{error, Reason} -> erlang:display("You cannot connect to the monitor at this time.")
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private helper funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% returns my IP address
local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, Addr =/= {127,0,0,1}]).

connect(IP_address, Port) ->

	case gen_tcp:connect(IP_address, Port, [binary, {packet, 4}, {active, false}]) of
		{ok, Socket} -> {ok, Socket};
		{error, Reason} -> {error, Reason}
	end.

upload(FilePacket, Peers) ->

	erlang:display("uploading file to my peers!"),

	% parse file packet 
	{Filename, Hash, Content} = parse_packet(FilePacket),

	% build upload request packet
	Packet = term_to_binary({upload, Filename, Hash, Content}),

	upload_to_peer(Peers, Packet).


download(Hash, Peers, Filename) ->

	erlang:display("downloading my file from my peers!"),

	% build download request packet
	Packet = term_to_binary({download, Filename, Hash}),

	download_from_peer(Peers, Packet, Filename).

upload_to_peer([], Packet) -> 0;
upload_to_peer([H | T], Packet) ->
	{IP, Port} = H,

	case connect(IP, Port) of
		{ok, Socket} ->

			gen_tcp:send(Socket, Packet),

			gen_tcp:close(Socket); 

		{error, Reason} -> erlang:display("You cannot connect to a peer.")
	end,
	upload_to_peer(T, Packet).

download_from_peer([], Packet, Filename) -> 0;
download_from_peer([H | T], Packet, Filename) ->
	{IP, Port} = H,

	case connect(IP, Port) of
		{ok, Socket} ->

			gen_tcp:send(Socket, Packet),

			%wait for response
			{ok, RetVal} = gen_tcp:recv(Socket, 0),

			erlang:display("received a packet from a peer!"),

			gen_tcp:close(Socket),

			% TODO: this exception handler is not working
			case RetVal of
				"Error: file not found" -> 
					erlang:display("peer does not have your file");
				MyFile -> 
					% write file
					Success = file:write_file(filename:basename(Filename), RetVal),

					case Success of
						ok -> erlang:display("OK");
						{error, Reason} -> erlang:display(Reason)
					end
			end;

			%erlang:display(Filename),
			%erlang:display(filename:basename(Filename)),
			%erlang:display(filename:join(["./", filename:basename(Filename)])),

			% write file
			%Success = file:write_file(filename:basename(Filename), RetVal),

			%case Success of
			%	ok -> erlang:display("OK");
			%	{error, Reason} -> erlang:display(Reason)
			%end;

		{error, Reason} -> 
			erlang:display("You cannot connect to a peer."),
			download_from_peer(T, Packet, Filename)
	end.


















