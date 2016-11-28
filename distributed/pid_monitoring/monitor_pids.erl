-module(monitor_pids).
-export([connect_client/2, start_monitoring/2]).

connect_client(Node, Pid) ->
    NewPid = rpc:call(Node, erlang, list_to_pid, [Pid]),
    start_monitoring(Node, NewPid).

start_monitoring(Node, ServPid) ->
	MonitorRef = monitor(process, ServPid).
