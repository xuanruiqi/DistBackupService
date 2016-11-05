%%%-------------------------------------------------------------------
%%% @author Benjamin E. Holen <bholen01@vm-hw01.eecs.tufts.edu>
%%% @copyright (C) 2016, Benjamin E. Holen
%%% @doc
%%%
%%% @end
%%% Created :  5 Nov 2016 by Benjamin E. Holen <bholen01@vm-hw01.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Child1 = #{id => 'Server1',
	       start => {basicS, start_link, ['Server1']},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker},

    Child2 = #{id => 'Server2',
	       start => {basicS, start_link, ['Server2']},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker},

    Child3 = #{id => 'Server3',
	       start => {basicS, start_link, ['Server3']},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker},


    {ok, {SupFlags, [Child1, Child2, Child3]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
