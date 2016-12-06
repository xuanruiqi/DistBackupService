-module(catch_error).
-import(tcp_sup, [start_link/0, start_link/1]).
-export([start_server/0, catch_bad_val/0]).

start_server() ->
	try start_link() of
		Any -> Any
	catch
		_:_ -> erlang:display("error occured")
		%{badmatch,{error,eaddrinuse}} -> erlang:display("port not free")
	end.

catch_bad_val() ->
	try divide_by_zero() of
		Any -> Any
	catch 
		_:_ -> erlang:display("cant divide by zero")
	end.

divide_by_zero() ->

	5/0. 




