-module(monitor_tcp_server).
-behavior(gen_server).

-import(file_proc, [parse_packet/1]).
-import(database, [lookup_peers/2]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

%%
%% Start a monitor's TCP server on a socket
%%
start_link(Socket) ->
    % server name is ?MODULE
    gen_server:start_link(?MODULE, Socket, []).


%%
%% Initiate the TCP server
%%
init(Socket) ->
    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.


%%
%% Stop the TCP server
%%
stop() ->
    gen_server:cast(?MODULE, stop).


%%
%% Handle a connection
%%
handle_cast(accept, State = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Boot a new listener to replace this one.
    monitor_tcp_sup:start_socket(),
    {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
    {noreply, State}.

%%
%% Handle TCP packets
%%
handle_info({tcp, Socket, <<"quit", _/binary>>}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, Packet}, State) ->
    erlang:display("received packet from client"),
    erlang:display(binary_to_term(Packet)),
    open_packet(Socket, binary_to_term(Packet)),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

%% Handle a call, which is not responded to.
handle_call(_E, _From, State) -> {noreply, State}.

%% Terminate & code change are not implemented
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%%
%% Handle a join request packet
%%
open_packet(Socket, {join, ClientNode, ClientServPid, ClientIP, ClientPort}) ->
    MonitorListener = whereis(listener),
    MonitorListener ! {connect, ClientNode, ClientServPid, ClientIP, ClientPort};

%%
%% Handle a logout request packet
%%
open_packet(Socket, {logout, ClientNode, ClientServPid}) ->
    MonitorListener = whereis(listener),
    MonitorListener ! {logout, ClientNode, ClientServPid};

%%
%% Handle a upload request packet
%%
open_packet(Socket, {upload, ClientNode, ClientServPid, Hash, ClientIP}) ->
    Peers = lookup_peers(ClientNode, ClientIP),
    gen_tcp:send(Socket, term_to_binary(Peers));


%%
%% Handle a download request packet
%%
open_packet(Socket, {download, ClientNode, ClientServPid, Hash, ClientIP}) ->
    Peers = lookup_peers(ClientNode, ClientIP),
    % send Peers back to client
    gen_tcp:send(Socket, term_to_binary(Peers)).

    

%% Send a message back to the client
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, true}]),
  ok.