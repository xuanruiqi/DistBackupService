-module(tcp_server).

%% API
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8099).

-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% server init funs
%%%===================================================================

start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
	start_link(?DEFAULT_PORT).


%%%===================================================================
%%% client funs
%%%===================================================================

get_count() ->
	gen_server:call(?SERVER, get_count).

stop() ->
	gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% init server
init([Port]) ->

	% create gen_tcp listening socket
	% an active socket will forward all incoming data as messages
	% to the process taht created it
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}]),

	% return 3-tupe with atom ok, proc state, and 0 timeout value
	% 0 timeout value forces you to handle a timeout message 
	% (in handle_info/2) as the first thing you do after initialization
	{ok, #state{port = Port, lsock = ListenSocket}, 0}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->

	do_rpc(Socket, RawData),
	RequestCount = State#state.request_count,
	{noreply, State#state{request_count = RequestCount + 1}};


% this is the first thing the server does after it has finished
% running the init/1 function
handle_info(timeout, #state{lsock = ListenSocket} = State) ->

	%use gen_tcp:accept/1 to wait for 
	% a TCP connection on your listening socket
	{ok, _Sock} = gen_tcp:accept(ListenSocket),

	%returns and signals to the gen_server container that 
	% you want to continue as normal with an unchanged state
	{noreply, State}.

handle_call(get_count, _From, State) ->

	% reply indicates I want to send a reply to the client

	% value returned to to client will be {ok, N}
	% where N is the current number of requests

	% set new server state to same as before
	{reply, {ok, State#state.request_count}, State}.

terminate(_Reason, _State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_rpc(Socket, RawData) ->
	gen_tcp:send(Socket,  io_lib:fwrite("Hello ~p~n", [RawData])).




