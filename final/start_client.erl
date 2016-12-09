-module(start_client).

-export([main/1]).

-import(client, [join/3, logout/2, init_download/3, init_upload/3]).
-define(MONITOR_PORT, 8099).
-define(SELF_PORT, 8098).
-mode(compile).

%%
%% Run the client
%%
main([Command | _]) -> 
    case Command of
        "start" -> join();
        _       -> usage(),
                   halt(1)
    end;
main([]) -> usage().


%%
%% Join the cluster
%%
join() -> 
    case file:open("ip.conf", read) of
                {ok, ConfigFile} -> 
                    IPRaw = io:get_line(ConfigFile, "~s"),
                            % http://stackoverflow.com/questions/12794358/how-to-strip-all-blank-characters-in-a-string-in-erlang
                            IP = re:replace(IPRaw, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    case inet:parse_address(IP) of
                          	{ok, MonitorIP}  -> MonitorIP,
                                        client:join(MonitorIP, ?MONITOR_PORT, ?SELF_PORT),
                                        loop(MonitorIP);
                          	{error, _} -> io:format("Invalid IP address~n"),
                                        halt(1)
                    end;
                     
                {error, _}  -> io:format("Error when reading config file~n"),
                                halt(1)
    end.

%%
%% Loop & wait for user to request upload, download or login.
%%
loop(IPAddr) -> 
    case io:get_line("> ") of
        "download\n" -> RawFilename = io:get_line("Filename: "),
                        Filename = re:replace(RawFilename, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                        io:fwrite("Filename: ~s", [Filename]),
                        client:init_download(IPAddr, ?MONITOR_PORT, Filename),
                        loop(IPAddr);
        "upload\n"   -> RawFilename = io:get_line("Filename: "), 
                        Filename = re:replace(RawFilename, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                        io:fwrite("Filename: ~s", [Filename]),
                        client:init_upload(IPAddr, ?MONITOR_PORT, Filename),
                        loop(IPAddr);
        "logout\n"   -> client:logout(IPAddr, ?MONITOR_PORT),
                        io:format("Bye!~n");
        _            -> io:format("Enter \"upload\", \"download\", or \"logout\"~n"),
                        loop(IPAddr)
    end.

%%
%% Show usage
%%
usage() -> 
    io:format("Usage: client start~n"),
    halt(1).