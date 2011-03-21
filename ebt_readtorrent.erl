-module(ebt_readtorrent).

-export([read/1]).

-include("ebt_torrent.hrl").

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            case ebt_bencode:decode(Binary) of
                {ok, Data} ->
                    {ok, load_torrent(Data)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

load_torrent({dict, Dict}) ->
    Keys = dict:fetch_keys(Dict),
    load_torrent(Keys, Dict, #torrent{});
load_torrent(_) ->
    #torrent{}.
load_torrent([<<"info">> | T], Dict, Torrent) ->
    case dict:find(<<"info">>, Dict) of
        {ok, Value} ->
            load_torrent(T, Dict, load_info(Value, Torrent));
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"announce">> | T], Dict, Torrent) ->
    case dict:find(<<"announce">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            load_torrent(T, Dict, Torrent#torrent{announce = Value});
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"announce-list">> | T], Dict, Torrent) ->
    case dict:find(<<"announce-list">>, Dict) of
        {ok, {list, Value}} when is_list(Value) ->
            load_torrent(T, Dict, load_announce_list(Value, Torrent));
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"comment">> | T], Dict, Torrent) ->
    case dict:find(<<"comment">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            load_torrent(T, Dict, Torrent#torrent{comment = Value});
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"creation date">> | T], Dict, Torrent) ->
    case dict:find(<<"creation date">>, Dict) of
        {ok, Value} when is_integer(Value) ->
            load_torrent(T, Dict, Torrent#torrent{creation_date = Value});
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"created by">> | T], Dict, Torrent) ->
    case dict:find(<<"created by">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            load_torrent(T, Dict, Torrent#torrent{encoding = Value});
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([<<"encoding">> | T], Dict, Torrent) ->
    case dict:find(<<"encoding">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            load_torrent(T, Dict, Torrent#torrent{encoding = Value});
        _ ->
            load_torrent(T, Dict, Torrent)
    end;
load_torrent([_ | T], Dict, Torrent) ->
    load_torrent(T, Dict, Torrent);
load_torrent([], _, Torrent) ->
    Torrent.

load_announce_list([{list, H} | T], Torrent) when is_list(H) ->
    NewList = [H | Torrent#torrent.announce_list],
    load_announce_list(T, Torrent#torrent{announce_list = NewList});
load_announce_list([_ | T], Torrent) ->
    load_announce_list(T, Torrent);
load_announce_list([], Torrent) ->
    Torrent.

load_info({dict, Dict}, Torrent) ->
    Keys = dict:fetch_keys(Dict),
    load_info(Keys, Dict, #torrent_info{}, Torrent);
load_info(_, Torrent) ->
    Torrent.
load_info([<<"piece length">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"piece length">>, Dict) of
        {ok, Value} when is_integer(Value) ->
            NewInfo = Info#torrent_info{piece_length = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"pieces">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"pieces">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            NewInfo = Info#torrent_info{pieces = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"private">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"private">>, Dict) of
        {ok, Value} when is_integer(Value) ->
            NewInfo = Info#torrent_info{private = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"name">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"name">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            NewInfo = Info#torrent_info{name = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"length">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"length">>, Dict) of
        {ok, Value} when is_integer(Value) ->
            NewInfo = Info#torrent_info{length = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"md5sum">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"md5sum">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            NewInfo = Info#torrent_info{md5sum = Value},
            load_info(T, Dict, NewInfo, Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([<<"files">> | T], Dict, Info, Torrent) ->
    case dict:find(<<"files">>, Dict) of
        {ok, Value} ->
            load_info(T, Dict, load_files(Value, Info), Torrent);
        _ ->
            load_info(T, Dict, Info, Torrent)
    end;
load_info([_ | T], Dict, Info, Torrent) ->
    load_info(T, Dict, Info, Torrent);
load_info([], _, Info, Torrent) ->
    Torrent#torrent{info = Info}.

load_files({list, List}, Info) when is_list(List) ->
    load_files(List, [], Info);
load_files(_, Info) ->
    Info.
load_files([{dict, Dict} | T], Files, Info) ->
    File = load_file(Dict),
    load_files(T, [File | Files], Info);
load_files([_ | T], Files, Info) ->
    load_files(T, Files, Info);
load_files([], Files, Info) ->
    Info#torrent_info{files = Files}.

load_file(Dict) ->
    Keys = dict:fetch_keys(Dict),
    load_file(Keys, Dict, #torrent_file{}).
load_file([<<"length">> | T], Dict, File) ->
    case dict:find(<<"length">>, Dict) of
        {ok, Value} when is_integer(Value) ->
            load_file(T, Dict, File#torrent_file{length = Value});
        _ ->
            load_file(T, Dict, File)
    end;
load_file([<<"path">> | T], Dict, File) ->
    case dict:find(<<"path">>, Dict) of
        {ok, {list, Value}} when is_list(Value) ->
            load_file(T, Dict, File#torrent_file{path = Value});
        _ ->
            load_file(T, Dict, File)
    end;
load_file([<<"md5sum">> | T], Dict, File) ->
    case dict:find(<<"md5sum">>, Dict) of
        {ok, Value} when is_binary(Value) ->
            load_file(T, Dict, File#torrent_file{md5sum = Value});
        _ ->
            load_file(T, Dict, File)
    end;
load_file([_ | T], Dict, File) ->
    load_file(T, Dict, File);
load_file([], _, File) ->
    File.
