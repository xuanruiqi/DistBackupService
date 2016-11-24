-module(server_linker).
-export([start_proxy/1]).

start_proxy(Node) ->

	erlang:display("Spawning gen_tcp server sup on..."),
	erlang:display(Node),

	%ClientServ = proc_lib:spawn_opt(Node, tcp_sup, start_link, [], [monitor]),
	ClientServ = proc_lib:spawn_link(Node, tcp_sup, start_link, []), 
	
	erlang:display(ClientServ),
	erlang:display("Server spawned.").