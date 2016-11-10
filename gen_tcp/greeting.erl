-module(greeting).
-export([greet/0]).
-import(say_hello, [hello/0]).

greet() ->

	hello().

