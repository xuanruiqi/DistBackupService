-module(client).

-import(file_proc, [build_packet/1, parse_packet/1]).
-import(tcp_sup, [start_link/0, start_link/1]).
-export([join/3, logout/2, init_upload/3, init_download/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(MonitorIP, MonitorPort, MyPort) ->

	% TODO: write function to initialize client's db of file records
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
	FilePacket = build_packet(File),

	% parse file packet to get Hash
	{Filename, Hash, Content} = parse_packet(FilePacket),

	% TODO: write a function called add(Filename, Hash)
	% thats adds a new file record to the client's db
	% TODO: uncomment the line below
	% add(Filename, Hash),

	% build init_upload request packet
	Packet = term_to_binary({upload, node(), ServPid, Hash}),

	% send request to logout
	gen_tcp:send(Socket, Packet),

	% catch return value
	{ok, RetVal} = gen_tcp:recv(Socket, 0),

	% convert RetVal to list of Peers
	Peers = binary_to_term(RetVal),

	gen_tcp:close(Socket), 

	erlang:display(Peers).

	% call helper fun to upload File to every Peer in Peers

init_download(MonitorIP, MonitorPort, File) ->
	
	% get pid of my server
	ServPid = whereis(tcp_sup),

	% TODO: write function called lookup(File) 
	% that searches for File in the client's db
	% and returns the corresponding Hash
	% TODO: uncomment the line below
	% Hash = lookup(File),

	% TODO: delete the line below
	Hash = 0,

	% init tcp connection with monitor's server
	Socket = connect(MonitorIP, MonitorPort),

	% build init_download request packet
	Packet = term_to_binary({download, node(), ServPid, Hash}),

	% send request to logout
	gen_tcp:send(Socket, Packet),

	% catch return value
	{ok, RetVal} = gen_tcp:recv(Socket, 0),

	% convert RetVal to list of Peers
	Peers = binary_to_term(RetVal),

	gen_tcp:close(Socket),

	erlang:display(Peers).

	% call helper fun to download File from every Peer in Peers


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