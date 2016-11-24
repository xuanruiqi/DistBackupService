-module(starter).
-export([start_link/4]).

start_link(Mod, Arg, Opt, Node) ->
   gen_fsm:start_link(Node, none, Mod, Arg, Opt).