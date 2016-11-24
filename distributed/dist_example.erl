%%%
%%% From Cesarini and Thompson, Chapter 11, p. 248
%%%
%%% Used to prove that we can communicate between distributed
%%% Erlang nodes.
%%% 
%%% Looping/server added by Mark A. Sheldon, Tufts University, Fall 2016
%%% Want process to stay up so we can use the observer to see that we
%%% really started a process on another node.
%%%

-module(dist_example).
-export([t/1, loop/0]).

t(From) -> From ! node().

%%
%% All messages just contain the PID of the requesting process
%%
loop() ->
    receive
        From -> From ! node(),
                loop()
    end.