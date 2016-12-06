-module(client).

-import(file_proc, [build_packet/1, parse_packet/1, write_peer_file/2]).
-import(tcp_sup, [start_link/0, start_link/1]).
-import(database, [init_client_dets/0, add_file_to_table/1,
                   lookup_file/1]).
-export([join/3, logout/2, init_upload/3, init_download/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(MonitorIP, MonitorPort, MyPort) ->

	init_client_dets(),
	% Each record in the db should have key: Filename and value: Hash

	% spawn my tcp_server
	tcp_sup:start_link(MyPort),

	% find my IP address and Port

	MyIP = local_ip_v4(),

	% get pid of my server
	ServPid = whereis(tcp_sup),

	% init tcp connection with monitor's server
	Socket = connect(MonitorIP, MonitorPort),

	% build join request packet
	Packet = term_to_binary({join, node(), pid_to_list(ServPid), MyIP, MyPort}),

	% send request to join
	gen_tcp:send(Socket, Packet),

	gen_tcp:close(Socket).

logout(MonitorIP, MonitorPort) ->

	% get pid of my server
	ServPid = whereis(tcp_sup),

	% init tcp connection with monitor's server
	Socket = connect(MonitorIP, MonitorPort),

	% build logout request packet
	Packet = term_to_binary({logout, node(), ServPid}),

	% send request to logout
	gen_tcp:send(Socket, Packet),

	gen_tcp:close(Socket).

init_upload(MonitorIP, MonitorPort, File) ->

	% get pid of my server
	ServPid = whereis(tcp_sup),

	% init tcp connection with monitor's server
	Socket = connect(MonitorIP, MonitorPort),

	% build file packet
	% something is wrong in build_packet
	FilePacket = build_packet(File),

	% parse file packet to get Hash
	{Filename, Hash, Content} = parse_packet(FilePacket),

	add_file_to_table({Filename, Hash}),
	% thats adds a new file record to the client's db

	% build init_upload request packet
	Packet = term_to_binary({upload, node(), ServPid, Hash}),

	% send request to logout
	gen_tcp:send(Socket, Packet),

	% catch return value
	{ok, RetVal} = gen_tcp:recv(Socket, 0),

	% convert RetVal to list of Peers
	Peers = binary_to_term(RetVal),

	gen_tcp:close(Socket), 

	erlang:display(Peers),

	% call helper fun to upload File to every Peer in Peers
	upload(FilePacket, Peers).

init_download(MonitorIP, MonitorPort, File) ->
	
	% get pid of my server
	ServPid = whereis(tcp_sup),

	Hash = lookup_file(File),

	% init tcp connection with monitor's server
	Socket = connect(MonitorIP, MonitorPort),

	% build init_download request packet
	Packet = term_to_binary({download, node(), ServPid, Hash}),

	% send request to init_download
	gen_tcp:send(Socket, Packet),

	% catch return value
	{ok, RetVal} = gen_tcp:recv(Socket, 0),

	% convert RetVal to list of Peers
	Peers = binary_to_term(RetVal),

	gen_tcp:close(Socket),

	erlang:display(Peers),

	% call helper fun to download File from every Peer in Peers
	download(Hash, Peers).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private helper funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% returns my IP address
local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, Addr =/= {127,0,0,1}]).

connect(IP_address, Port) ->

	{ok, Socket} = gen_tcp:connect(IP_address, Port, [binary, {packet, 4}, {active, false}]),
	Socket.

upload(FilePacket, Peers) ->

	erlang:display("uploading file to my peers!"),

	% parse file packet 
	{Filename, Hash, Content} = parse_packet(FilePacket),

	% build upload request packet
	Packet = term_to_binary({upload, Filename, Hash, Content}),

	% TODO: write loop that connects client to each Peer in Peers
	% and sends Packet to each Peer
	upload_to_peer(Peers, Packet).


download(Hash, Peers) ->

	erlang:display("downloading my file from my peers!"),

	% build download request packet
	Packet = term_to_binary({download, Hash}),

	% TODO: write loop that connects client to each Peer in Peers
	% and sends Packet to each Peer
	download_from_peer(Peers, Packet).

upload_to_peer([], Packet) -> 0;
upload_to_peer([H | T], Packet) ->
	{IP, Port} = H,
	Socket = connect(IP, Port), 
	gen_tcp:send(Socket, Packet),
	gen_tcp:close(Socket),
	upload_to_peer(T, Packet).

download_from_peer([], Packet) -> 0;
download_from_peer([H | T], Packet) ->
	{IP, Port} = H,
	Socket = connect(IP, Port), 
	gen_tcp:send(Socket, Packet),

	% wait for response
	{ok, Packet} = gen_tcp:recv(Socket, 0),

	gen_tcp:close(Socket),

	% parse packet
	{Filename, Hash, Content} = parse_packet(Packet),

	% write file
	file:write_file(filename:join(["./", filename:basename(Filename)]), Content),

	download_from_peer(T, Packet).















