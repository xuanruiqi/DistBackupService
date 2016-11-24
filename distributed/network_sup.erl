
-module(network_sup).
-behaviour(supervisor).

init(_Args) ->
   StartArgs = [starter, [], []],
   StartFunc = {start_link, start_link, StartArgs},
   ChildSpec = {starter, StartFunc, transient, 4000, worker, [starter]},
   {ok, {{simple_one_for_one, 10, 60}, [ChildSpec]}}.

