%%% @author Benjamin E. Holen <bholen01@rm121-01.eecs.tufts.edu>
%%% @copyright (C) 2016, Benjamin E. Holen
%%% @doc
%%%
%%% @end
%%% Created :  2 Dec 2016 by Benjamin E. Holen

-module(database).

% monitor funcs
-export([init_monitor_dets/0, add_client/1, lookup_monitor_ref/1, lookup_client/1, 
	remove_client_from_database/1]).

% monitor_tcp_server funcs
-export([lookup_peers/1]).

% client funcs
-export([init_client_dets/0, add_file_to_table/1, lookup_file/1]).

open_table(monitor) ->
    dets:open_file('Database', [{file, 'Database.table'}]);

open_table(client) ->
    dets:open_file('Files', [{file, 'Files.table'}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONITOR DATABASE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_monitor_dets() ->
    open_table(monitor).


add_client(Entry = {_ClientNode, _MonitorRef, _NewClientServPid, _ClientIP, _ClientPort}) ->
    open_table(monitor),
    dets:insert('Database', Entry).

lookup_monitor_ref(ClientNode) ->
    open_table(monitor),
    case  dets:lookup('Database', ClientNode) of
        [{ClientNode, MonitorRef,_,_,_}] -> MonitorRef;
	_ -> {error, notfound}
    end.


lookup_client(MonitorRef) ->
    open_table(monitor),
    case dets:match_object('Database', {'_', MonitorRef, '_', '_', '_'}) of
        [{ClientNode, MonitorRef, _, _, _}] -> ClientNode;
	_ -> {error, notfound}
    end.

remove_client_from_database(ClientNode) ->
    open_table(monitor),
    dets:delete('Database', ClientNode).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONITOR DATABASE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_peers(Self) ->
    open_table(monitor),

    Peers = dets:foldr(fun (E, Acc) ->  [E|Acc] end, [], 'Database'),
    proplists:delete(Self, Peers), 
    Peers.
%    Peers = [{IP, Port} || {_,_,_, IP, Port} <- 
%	    dets:match_object('Database', {'_' ,'_','_','_','_'})],

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLIENT TABLE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



init_client_dets() ->
    open_table(client).

add_file_to_table(Entry = {_Filename, _Hash}) ->
    open_table(client),
    dets:insert('Files', Entry).

lookup_file(Filename) ->
    open_table(client),
    case  dets:lookup('Files', Filename) of
        [{Filename, Hash}] -> Hash;
	_ -> {error, notfound}
    end.

