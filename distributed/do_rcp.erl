-module(do_rcp).
-export([say_hello/0]).


say_hello() ->

	erlang:display("Hey there!").