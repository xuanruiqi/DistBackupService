-module(multi_server).

-export([
         start_link/1,
         start_link/0
         ]).

-export([init/1, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8099).

-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
	% ?SERVER is an atom
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
	start_link(?DEFAULT_PORT).

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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_info(timeout, #state{lsock = ListenSocket} = State) ->
	spawn(fun() -> acceptor(ListenSocket) end),
    {noreply, State};
handle_info({tcp, Socket, RawData}, State) ->
	do_rpc(Socket, RawData),
	RequestCount = State#state.request_count,
	{noreply, State#state{request_count = RequestCount + 1}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

acceptor(ListenSocket) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	erlang:display("connection made!"),
	spawn(fun() -> acceptor(ListenSocket) end).

do_rpc(Socket, RawData) ->
	gen_tcp:send(Socket,  io_lib:fwrite("Hello ~p~n", [RawData])).