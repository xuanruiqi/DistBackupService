-module(tcp_sup).
-behavior(supervisor).

-export([start_link/0, start_link/2, start_socket/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8099).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_link(Port, ParentPid) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, ParentPid]).

init([]) ->


	% {ok, ListenSocket} = gen_tcp:listen(?DEFAULT_PORT, [{active, true}, binary, {packet, 4}]),

	case gen_tcp:listen(?DEFAULT_PORT, [{active, true}, binary, {packet, 4}]) of
		{ok, ListenSocket} -> 
			%% We start our pool of empty listeners.
			%% We must do this in another, as it is a blocking process.

			spawn_link(fun empty_listeners/0),

			SupFlags = #{strategy => simple_one_for_one,
						intensity => 60,
						period => 3600},
			ChildSpecs = [#{id => tcp_server,
							start => {tcp_server, start_link, [ListenSocket]},
							restart => temporary,
							type => worker,
							shutdown => 1000,
							modules => [tcp_server]}],

			{ok, {SupFlags, ChildSpecs}};
		{error, Reason} -> 
			erlang:display("port bad"),
			init([?DEFAULT_PORT - 1])
	end;


init([Port, ParentPid]) ->

	case gen_tcp:listen(Port, [{active, true}, binary, {packet, 4}]) of
		{ok, ListenSocket} -> 

			%% When we have successfully started a listener on Port, 
			%% send Port back to ParentPid
			ParentPid ! Port,
			%% We start our pool of empty listeners.
			%% We must do this in another, as it is a blocking process.

			spawn_link(fun empty_listeners/0),

			SupFlags = #{strategy => simple_one_for_one,
						intensity => 60,
						period => 3600},
			ChildSpecs = [#{id => tcp_server,
							start => {tcp_server, start_link, [ListenSocket]},
							restart => temporary,
							type => worker,
							shutdown => 1000,
							modules => [tcp_server]}],

			{ok, {SupFlags, ChildSpecs}};
		{error, Reason} -> 
			erlang:display("port bad"),
			init([Port - 1, ParentPid])
	end.

start_socket() ->
	supervisor:start_child(?MODULE, []).


%% Spawn 20 listeners so that many multiple connections can
%% be started at once
empty_listeners() ->
	erlang:display("SUPERVISOR: spawning 20 child acceptors"),
	[start_socket() || _ <- lists:seq(1,20)],
	ok.