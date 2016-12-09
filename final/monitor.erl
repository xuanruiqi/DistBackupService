-module(monitor).

-import(monitor_tcp_sup, [start_link/0, start_link/1]).
-import(database, [init_monitor_dets/0, add_client/1, lookup_monitor_ref/1,
            lookup_client/1, remove_client_from_database/1, clear_table/0]).

-export([init_monitor/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monitor Init Funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Initiate a monitor. If called with [], the monitor server listens 
%% on the standard port, 8099. If called with [Port], the monitor server 
%% listens on a nonstandard port.
init_monitor([]) ->
    database:clear_table(),
    init_monitor_dets(),
    % register process
    register(monitor, self()),
    % start tcp_sup (effectively starting a tcp server)
    start_link(),
    % start listening for messages from my tcp server
    ListenerPid = spawn(fun() -> listen() end),
    register(listener, ListenerPid);

init_monitor([Port]) ->
    init_monitor_dets(),
    % register process
    register(monitor, self()),
    % start monitor_tcp_sup (effectively starting a tcp server)
    start_link(Port),
    % start listening for messages from my tcp server
    ListenerPid = spawn(fun() -> listen() end),
    register(listener, ListenerPid).



%%
%% Listen to a message sent to the monitor
%%
listen() ->
    receive
        {message, Msg} -> 
            listen();
        {connect, ClientNode, ClientServPid, ClientIP, ClientPort} ->
            connect_client(ClientNode, ClientServPid, ClientIP, ClientPort),
            listen();
        {logout, ClientNode, ClientServPid} ->
            logout_client(client_left, ClientNode, ClientServPid),
            listen();
        {'DOWN', MonitorRef, Type, Object, Info} ->
            logout_client(client_down, MonitorRef),
            listen()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monitor Customer Service Funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Connect a client that requested to join the cluster
%%
connect_client(ClientNode, ClientServPid, ClientIP, ClientPort) ->

    io:format("LOG: CLIENT REQUESTED JOIN: adding client.~n"),
    % connect to ClientNode
    % Test this without ping
    net_adm:ping(ClientNode),
    NewClientServPid = rpc:call(ClientNode, erlang, list_to_pid, [ClientServPid]),
    MonitorRef = monitor(process, NewClientServPid),
    DatabaseEntry = {ClientNode, MonitorRef, NewClientServPid,
                     ClientIP, ClientPort},
    add_client(DatabaseEntry).

%%
%% Log a client out from the cluster if the client requests so
%%
logout_client(client_left, ClientNode, ClientServPid) ->
    io:format("LOG: CLIENT REQUESTED LOGOUT: logging out client~n"),
    MonitorRef = lookup_monitor_ref(ClientNode),
    remove_client_from_database(ClientNode),
    demonitor(MonitorRef),
    % tell client to kill his server
    rpc:call(ClientNode, erlang, exit, [ClientServPid, kill]).

logout_client(client_down, MonitorRef) ->
    io:format("LOG: CLIENT DOWN: logging out client~n"),
    ClientNode = lookup_client(MonitorRef),
    remove_client_from_database(ClientNode).
