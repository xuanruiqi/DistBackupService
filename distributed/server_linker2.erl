-module(server_linker2).

-export([init/1]).

init([Node]) ->

	ClientServ = spawn_link(Node, tcp_sup, start_link, []).