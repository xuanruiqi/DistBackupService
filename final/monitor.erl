-module(monitor).

-import(monitor_tcp_sup, [start_link/0, start_link/1]).
-import(database, [init_monitor_dets/0, add_client/1, lookup_monitor_ref/1,
			lookup_client/1, remove_client_from_database/1]).

-export([init_monitor/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monitor Init Funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_monitor([]) ->

	init_monitor_dets(),

	% register process
	register(monitor, self()),

	% start tcp_sup (effectively starting a tcp server)
	start_link(),

	% start listening for messages from my tcp server
	ListenerPid = spawn(fun() -> listen() end),

	erlang:display(ListenerPid),

	register(listener, ListenerPid);

init_monitor([Port]) ->

	init_monitor_dets(),

	% register process
	register(monitor, self()),

	% start monitor_tcp_sup (effectively starting a tcp server)
	start_link(Port),

	% start listening for messages from my tcp server
	ListenerPid = spawn(fun() -> listen() end),

	erlang:display(ListenerPid),

	register(listener, ListenerPid).

listen() ->

	receive
		{message, Msg} -> erlang:display(Msg),
			listen();
		{connect, ClientNode, ClientServPid, ClientIP, ClientPort} ->
			connect_client(ClientNode, ClientServPid, ClientIP, ClientPort),
			listen();
		{logout, ClientNode, ClientServPid} ->
			logout_client(client_left, ClientNode),
			listen();
		{'DOWN', MonitorRef, Type, Object, Info} ->
			logout_client(client_down, MonitorRef),
			listen()
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monitor Customer Service Funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_client(ClientNode, ClientServPid, ClientIP, ClientPort) ->

	erlang:display("CLIENT REQUESTED JOIN: adding client"),

	NewClientServPid = rpc:call(ClientNode, erlang, list_to_pid, [ClientServPid]),

	erlang:display(NewClientServPid),

	MonitorRef = monitor(process, NewClientServPid),

	DatabaseEntry = {ClientNode, MonitorRef, NewClientServPid,
			 ClientIP, ClientPort},
		% ClientNode (key),
		% MonitorRef (we will need this later in logout_client()),
		% NewClientServPid,
		% ClientIP, 
		% ClientPort
	add_client(DatabaseEntry).


logout_client(client_left, ClientNode) ->

        erlang:display("CLIENT REQUESTED LOGOUT: logging out client"),
    

        MonitorRef = lookup_client(ClientNode),

        remove_client_from_database(ClientNode),
    
	demonitor(MonitorRef);

logout_client(client_down, MonitorRef) ->

	erlang:display("CLIENT DOWN: logging out client"),

	% using client's MonitorRef
	ClientNode = lookup_monitor_ref(MonitorRef),

	remove_client_from_database(ClientNode).

	% there is no need to call demonitor() in this case
	% because monitoring was turned off when client went down











