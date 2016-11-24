-module(monitor).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1, startServer/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	RemoteNode = 'banana@Brinleys-MacBook-Air',

    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,
		 period => 5},
	ChildSpecs = [#{id => server_linker,
					start => {server_linker, start_proxy, [RemoteNode]},
					restart => temporary,
					type => worker,
					shutdown => 1000,
					modules => [server_linker]}],

	{ok, {SupFlags, ChildSpecs}}.


startServer(Node) ->

	erlang:display("MONITOR: starting proxy sup on my node.."),

	supervisor:start_child(?MODULE, []).
