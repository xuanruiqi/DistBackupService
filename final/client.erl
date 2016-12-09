-module(client).

-import(file_proc, [build_packet/1, parse_packet/1, write_peer_file/2]).
-import(tcp_sup, [start_link/0, start_link/2]).
-import(database, [init_client_dets/0, add_file_to_table/1,
                   lookup_file/1]).
-export([join/3, logout/2, init_upload/3, init_download/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Joins a cluster of peers that is monitored at MonitorIP on port 
%% MonitorPort. It is the client's responsibility to make sure 
%% that MyPort is free; if MyPort is not free, then a free 
%% port is used.
%%
join(MonitorIP, MonitorPort, MyPort) ->

    init_client_dets(),

    MyPid = self(),

    % start my tcp_server
    tcp_sup:start_link(MyPort, MyPid),

    % wait for tcp_sup to send back the listening port
    receive
        MyNewPort -> MyNewPort
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
        {error, _} -> 
            io:format("You cannot connect to the monitor at this time.~n"),
            exit(whereis(tcp_sup), kill)
    end.

%%
%% Log out from a cluster monitored at MonitorIP on MonitorPort.
%%
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
        {error, Reason} -> 
            io:format("You cannot connect to the monitor at this time.~n")
    end.

%%
%% Initiate upload of a file to a cluster monitored at MonitorIP on MonitorPort, 
%% where File is the path to the file to be uploaded.
%%
init_upload(MonitorIP, MonitorPort, File) ->

    % get pid of my server
    ServPid = whereis(tcp_sup),

    % find my IP address
    MyIP = local_ip_v4(),

    case build_packet(File) of
        nonexistent_file -> 
            io:format("Your file does not exist~n");
        FilePacket ->
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
                            io:format("You cannot connect to the monitor at this time.~n");
                        List -> 
                            % call helper fun to upload File to every Peer in Peers
                            upload(FilePacket, Peers)
                    end;

                    % call helper fun to upload File to every Peer in Peers
                    %upload(FilePacket, Peers);
                {error, Reason} -> 
                    io:format("You cannot connect to the monitor at this time.~n")
            end
    end.

%%
%% Initiate download of a file from a cluster monitored at MonitorIP on MonitorPort, 
%% where File is the path to the file to be downloaded.
%%
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

%%
%% Returns my IP address
%%
local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, Addr =/= {127,0,0,1}]).

%%
%% Connect to a node at an IP address on a port
%%
connect(IPAddress, Port) ->
    case gen_tcp:connect(IPAddress, Port, [binary, {packet, 4}, {active, false}]) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.

%% 
%% Builds a file packet and upload the file to each peer in the cluster
%%
upload(FilePacket, Peers) ->
    % parse file packet 
    {Filename, Hash, Content} = parse_packet(FilePacket),
    % build upload request packet
    Packet = term_to_binary({upload, Filename, Hash, Content}),
    upload_to_peer(Peers, Packet).

%% 
%% Builds a file packet and download file from a peer in the cluster
%%
download(Hash, Peers, Filename) ->
    % build download request packet
    Packet = term_to_binary({download, Filename, Hash}),
    download_from_peer(Peers, Packet, Filename).


%% 
%% Send a file upload packet to a peer node
%%
upload_to_peer([], Packet) -> 0;
upload_to_peer([H | T], Packet) ->
    {IP, Port} = H,

    case connect(IP, Port) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Packet),
            gen_tcp:close(Socket); 
        {error, Reason} -> 
            io:format("You cannot connect to a peer.~n")
    end,
    upload_to_peer(T, Packet).


%% 
%% Send a file download packet to a peer node
%%
download_from_peer([], Packet, Filename) -> 0;
download_from_peer([H | T], Packet, Filename) ->
    {IP, Port} = H,

    case connect(IP, Port) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Packet),
            % wait for response
            {ok, RetVal} = gen_tcp:recv(Socket, 0),
            gen_tcp:close(Socket),
            case RetVal of
                <<"Error: file not found">> -> 
                    io:format("The peer does not have your file. Trying with another peer...~n"),
                    download_from_peer(T, Packet, Filename);
                <<"Error: no files uploaded yet">> ->
                    io:format("You have not uploaded a file yet!~n");
                MyFile -> 
                    % write the file
                    Success = file:write_file(filename:basename(Filename), RetVal),
                    case Success of
                        ok -> io:format("Downloaded.~n");
                        {error, Reason} -> io:format("Error when writing file.~n")
                    end
            end;

        {error, Reason} -> 
            io:format("You cannot connect to the peer at this time. Trying with another peer...~n"),
            download_from_peer(T, Packet, Filename)
    end.


















