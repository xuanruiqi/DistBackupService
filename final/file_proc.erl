-module(file_proc).
-export([build_packet/1, parse_packet/1, write_peer_file/2, read/1]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       Exported Functions         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Build a file upload packet from the file at path Filename.
%% If the file is nonexistent or could not be read, return 
%% the atom 'nonexistent_file'.
%%
build_packet(Filename) ->
    case read(Filename) of
        error   -> 
            nonexistent_file;
        Content -> 
            Fname = pack_file_name(Filename),
            Hash  = md5(Content),
            <<Fname/binary, Hash/binary, Content/binary>>
    end.

%%
%% Parse a file packet, returning a tuple.
%%
parse_packet(Packet) -> 
    {Fname, Hash, Content} = split_packet(Packet),
    Filename = extract_filename(Fname),
    {Filename, Hash, Content}.

%%
%% Write a file from a peer to the peer_files directory.
%% If the directory /peer_files could not be created, 
%% the atom "error" is returned.
%%
write_peer_file(Filename, File) ->
    case filelib:ensure_dir("peer_files") of
        ok    -> file:make_dir("peer_files"),
                 file:write_file(filename:join(["./peer_files", Filename]), File);
        Error -> error
    end.

%%
%% Read a file at path "Filename". If the file could not 
%% be read or does not exist, the atom 'error' is returned.
%% 
read(Filename) -> 
    case file:read_file(Filename) of
        {ok, Content} -> Content;
        {error, _}    -> error
    end.

%%%
%%% Private Functions
%%%
%%
%% MD5 hash a file
%%
md5(File) -> crypto:hash(md5, File).

%%
%% Pad a file name with 0 bytes, creating a 225-byte binary.
%% UNIX file names are <= 255 bytes,so packing names into 255 bytes is fine
%% File names can't have the null ('\0') character in them (because they 
%% are strings), so it's safe to pad file names with 0s. 
%%
pack_file_name(Filename) ->
    FilenameBin = binary:list_to_bin(Filename),
    case byte_size(FilenameBin) of
        N when N < 255 -> <<FilenameBin/binary, 0:((255-N) * 8)>>;
        255            -> FilenameBin
    end. % Always packed into 255 bytes

%%
%% Split a file into three parts.
%%
split_packet(Packet) ->
    % Really tricky!! 255/binary means 255 bytes while 255 means 
    % 255 bits. This caught me.
    <<Fname:255/binary, Hash:16/binary, Content/binary>> = Packet,
    {Fname, Hash, Content}.

%%
%% Extract a file name from a 225-byte binary.
%%
extract_filename(Fname) ->
    FnameExtract = hd(binary:split(Fname, <<0>>, [trim])),
    binary:bin_to_list(FnameExtract).

