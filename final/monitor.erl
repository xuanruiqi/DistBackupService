-module(monitor).

-import(monitor_tcp_sup, [start_link/0, start_link/1]).

-export([init_monitor/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Monitor Init Funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_monitor([]) ->

	% TODO: init ets db

	% register process
	register(monitor, self()),

	% start tcp_sup (effectively starting a tcp server)
	start_link(),

	% start listening for messages from my tcp server
	ListenerPid = spawn(fun() -> listen() end),

	erlang:display(ListenerPid),

	register(listener, ListenerPid);

init_monitor([Port]) ->

	% TODO: init ets db

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

	NewClientServPid = rpc:call(ClientNode, erlang, list_to_pid, [ClientServPid]),

	erlang:display(NewClientServPid),

	MonitorRef = monitor(process, NewClientServPid).

	% TODO: add client to db with following attributes:
		% ClientNode (key),
		% MonitorRef (we will need this later in logout_client()),
		% NewClientServPid,
		% ClientIP, 
		% ClientPort


logout_client(client_left, ClientNode) ->

	erlang:display("CLIENT REQUESTED LOGOUT: logging out client");

	% TODO: write function to lookup ClientNode's ClientServPid in db
	% MonitorRef = lookup_monitor_ref(ClientNode)

	% TODO: write function to remove client from db
	% remove(ClientNode)

	% TODO: after lookup and remove funs have been implemented, 
	% uncomment the code below
	% demonitor(MonitorRef);

logout_client(client_down, MonitorRef) ->

	erlang:display("CLIENT DOWN: logging out client").

	% TODO: write function to lookup the client's ClientNode 
	% using client's MonitorRef
	% ClientNode = lookup_client_node(MonitorRef)

	% TODO: write function to remove client from db
	% remove(ClientNode).

	% there is no need to call demonitor() in this case
	% because monitoring was turned off when client went down











