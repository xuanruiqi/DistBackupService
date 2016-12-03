-module(file_proc).
-export([build_packet/1, parse_packet/1, write_peer_file/2]).

%%        Message Protocol
%% ==============================
%% | Client ID    | 8   bytes   |
%% | Message Type | 1   byte    |
%% | Payload      | the rest    |
%% ==============================
%%          File Uploads
%% ==============================
%% | Filename     | 255 bytes   |
%% | MD5 Hash     | 16  bytes   | 
%% | Content      | the rest    |
%% ==============================
%%          File Requests
%% ==============================
%% | File Number  | 16  bytes   |
%% ==============================
%%          File Listing
%% ==============================
%% | N/A          | N/A         |

read(Filename) -> 
    case file:read_file(Filename) of
        {ok, Content} -> Content;
        {error, _}    -> error
    end.

% MD5 hash is always 16 bytes
md5(File) -> crypto:hash(md5, File).

% UNIX file names are <= 255 bytes,so packing names into 255 bytes is fine
% File names can't have the null ('\0') character in them (because they 
% are strings), so it's safe to pad file names with 0s. 
pack_file_name(Filename) ->
    FilenameBin = binary:list_to_bin(Filename),
    case byte_size(FilenameBin) of
        N when N < 255 -> <<FilenameBin/binary, 0:((255-N) * 8)>>;
        255            -> FilenameBin
    end. % Always packed into 255 bytes

build_packet(Filename) ->
    Fname   = pack_file_name(Filename),
    Content = read(Filename),
    Hash    = md5(Content),
    <<Fname/binary, Hash/binary, Content/binary>>.

split_packet(Packet) ->
    % Really tricky!! 255/binary means 255 bytes while 255 means 
    % 255 bits. This caught me.
    <<Fname:255/binary, Hash:16/binary, Content/binary>> = Packet,
    {Fname, Hash, Content}.

extract_filename(Fname) ->
    FnameExtract = hd(binary:split(Fname, <<0>>, [trim])),
    binary:bin_to_list(FnameExtract).

parse_packet(Packet) -> 
    {Fname, Hash, Content} = split_packet(Packet),
    Filename = extract_filename(Fname),
    {Filename, Hash, Content}.

write_peer_file(Filename, File) ->
    erlang:display("writing peer file"),
    case filelib:ensure_dir("peer_files") of
        ok    -> file:make_dir("peer_files"),
                 file:write_file(filename:join(["./peer_files", Filename]), File);
        Error -> Error
    end.
