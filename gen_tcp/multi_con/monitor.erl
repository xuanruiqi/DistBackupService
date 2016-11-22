%%%-------------------------------------------------------------------
%%% @author Benjamin E. Holen <bholen01@vm-hw06.eecs.tufts.edu>
%%% @copyright (C) 2016, Benjamin E. Holen
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2016 by Benjamin E. Holen <bholen01@vm-hw06.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(monitor).

-behaviour(supervisor).

%% API
-export([start_link/0, login/1, logout/1, establishConnection/2]).
-export([startServer/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

login(Node) ->
    ets:insert(clients, {Node, []}),
    ets:insert(servers, {Node}),
    startServer(Node),
    ok.

logout(Node) ->
    ets:delete(clients, Node),
    sendLogoutRequest(Node),
    % update redundancy measures
    terminateServer(Node),
    ok.

establishConnection(Node, Request) ->
    forwardRequest(Node, Request),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, monitor}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    createEts(),
%   AChild = #{id => 'AName',
%	       start => {'AModule', start_link, []},
%	       restart => permanent,
%	       shutdown => 5000,
%	       type => worker,
%	       modules => ['AModule']},

    {ok, {SupFlags, []}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
forwardRequest(Node, Request) ->
    case ets:tab2list(servers) of
	[] ->
	    'Error_No_Servers';
	[Server] -> 
	    serverRequest(Server, Node, Request);
	[Server | _] ->
	    serverRequest(Server, Node, Request)
    end,
    ok.

serverRequest(_Server, _Node, _Request) ->
    ok.

startServer(Node) ->
    NewChild = {Node,
		{'tcp_sup', start_link, []},
	        permanent,
	        5000,
	        worker,
	        ['tcp_sup']},
    supervisor:start_child(monitor, NewChild).

terminateServer(Node) ->
    ets:delete(servers, Node),
    % send terminate request
    % wait for reply
    supervisor:terminate_child(monitor, Node),
    ok.

sendLogoutRequest(Node) ->
    forwardRequest(Node, {logout}).

createEts() ->
    ets:new(clients, [public, named_table]),
    ets:new(servers, [public, named_table]),
    ok.


