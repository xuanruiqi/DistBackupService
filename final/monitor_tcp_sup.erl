-module(monitor_tcp_sup).
-behavior(supervisor).

-export([start_link/0, start_link/1, start_socket/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8099).

%%
%% Start TCP server supervisor. If called with arity 0, 
%% start on default port (8099). If called with arity 1, start on 
%% a custom port.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%%
%% Initialize TCP server supervisor
%%
init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?DEFAULT_PORT, [{active, true}, binary, {packet, 4}]),
    %% We start our pool of empty listeners.
    %% We must do this in another, as it is a blocking process.
    spawn_link(fun empty_listeners/0),
    SupFlags = #{strategy => simple_one_for_one,
                intensity => 60,
                period => 3600},
    ChildSpecs = [#{id => tcp_server,
                    start => {monitor_tcp_server, start_link, [ListenSocket]},
                    restart => temporary,
                    type => worker,
                    shutdown => 1000,
                    modules => [monitor_tcp_server]}],
    {ok, {SupFlags, ChildSpecs}};
init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, binary, {packet, 4}]),
    %% We start our pool of empty listeners.
    %% We must do this in another proc, as it is a blocking process.
    spawn_link(fun empty_listeners/0),
    SupFlags = #{strategy => simple_one_for_one,
                intensity => 60,
                period => 3600},
    ChildSpecs = [#{id => tcp_server,
                    start => {monitor_tcp_server, start_link, [ListenSocket]},
                    restart => temporary,
                    type => worker,
                    shutdown => 1000,
                    modules => [monitor_tcp_server]}],
    {ok, {SupFlags, ChildSpecs}}.


%%
%% Start a socket
%%
start_socket() ->
    supervisor:start_child(?MODULE, []).

%%
%% Spawn 20 listeners so that many multiple connections can
%% be started at once
%%
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.