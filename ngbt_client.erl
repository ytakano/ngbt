%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ngbt_client).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([read_torrent/2, print_torrent/1]).
-export([start_download/1, stop_download/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("ngbt_torrent.hrl").

-record(state, {torrent,
                info_hash,
                stat         = waiting,  % waiting, downloding
                trackers     = [],
                interval     = 300,
                min_interval = 300,
                complete     = 0,
                incomplete   = 0,
                peers,
                pid_pref,
                pid_tracker_client,
                pid_files,
                pid_pieces}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(PIDPref) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PIDPref) ->
    gen_server:start_link(?MODULE, [PIDPref], []).

%%--------------------------------------------------------------------
%% @doc
%% stop the process
%%
%% @spec stop(PID) -> ok
%% @end
%%--------------------------------------------------------------------
stop(PID) ->
    gen_server:cast(PID, stop).

%%--------------------------------------------------------------------
%% @doc
%% Read .torrent file
%%
%% @spec read_torrent(PID, File) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
read_torrent(PID, File) ->
    gen_server:call(PID, {read_torrent, File}).

%%--------------------------------------------------------------------
%% @doc
%% print the information in torrent
%%
%% @spec print_torrent(PID) -> ok
%% @end
%%--------------------------------------------------------------------
print_torrent(PID) ->
    gen_server:call(PID, print_torrent).

%%--------------------------------------------------------------------
%% @doc
%% start download
%%
%% @spec start_download(PID) -> ok
%% @end
%%--------------------------------------------------------------------
start_download(PID) ->
    gen_server:cast(PID, start_download).

%%--------------------------------------------------------------------
%% @doc
%% stop download
%%
%% @spec start_download(PID) -> ok
%% @end
%%--------------------------------------------------------------------
stop_download(PID) ->
    gen_server:cast(PID, stop_download).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([PIDPref]) ->
    {ok, #state{torrent = #torrent{}, pid_pref = PIDPref,
                peers = ets:new(peers, [set, private])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({read_torrent, File}, _From, State) ->
    {Reply, Torrent, Hash} = case ngbt_readtorrent:read(File) of
                                 {ok, Data, SHA1} ->
                                     {ok, Data, SHA1};
                                 {error, Reason} ->
                                     {{error, Reason}, State#state.torrent,
                                      undefined}
                             end,

    Trackers = case Reply of
                   ok ->
                       make_trackers(Torrent#torrent.announce,
                                     Torrent#torrent.announce_list,
                                     [Torrent#torrent.announce]);
                   _ ->
                       State#state.trackers
               end,

    PIDPieces = case Reply of
                    ok ->
                        Info = Torrent#torrent.info,
                        Num = trunc(byte_size(Info#torrent_info.pieces) / 20),
                        case ngbt_pieces:start_link(Num) of
                            {ok, P1} ->
                                P1;
                            _ ->
                                State#state.pid_pieces
                        end;
                    _ ->
                        State#state.pid_pieces
                end,

    PIDFiles = case Reply of
                   ok ->
                       case ngbt_files:start_link(Torrent#torrent.info,
                                                  PIDPieces) of
                           {ok, P2} ->
                               P2;
                           _ ->
                               State#state.pid_files
                       end;
                   _ ->
                       State#state.pid_files
               end,

    {reply, Reply, State#state{torrent = Torrent, info_hash = Hash,
                               trackers = Trackers, pid_files = PIDFiles,
                               pid_pieces = PIDPieces}};
handle_call(print_torrent, _From, State) ->
    print_torrent_info(State#state.torrent),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(start_download, State) when State#state.stat =:= waiting ->
    NewState = case start_down(State) of
                   {ok, PID} when is_pid(PID) ->
                       State#state{pid_tracker_client = PID,
                                   stat = downloading};
                   {error, _} ->
                       State
               end,

    {noreply, NewState};
handle_cast(stop_download, State) when State#state.stat =:= downloading ->
    stop_down(State),
    {noreply, State#state{pid_tracker_client = undefined,
                          stat = waiting}};
handle_cast(stop, State) ->
    ngbt_tracker_client:stop(State#state.pid_tracker_client),
    ngbt_files:stop(State#state.pid_files),
    ngbt_pieces:stop(State#state.pid_pieces),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({peers, _, ok, Body}, State)
  when State#state.stat =:= downloading ->
    case ngbt_bencode:decode(Body) of
        {ok, {dict, Res}} ->
            io:format("tracker responce:~n"),
            NewState = tracker_res_handler(Res, State),

            %% TODO: start timer for announce

            {noreply, NewState};
        _ ->
            ngbt_tracker_client:stop(State#state.pid_tracker_client),
            NewState = retry_down(State),
            {noreply, NewState}
    end;
handle_info({peers, _, error, _}, State)
  when State#state.stat =:= downloading ->
    ngbt_tracker_client:stop(State#state.pid_tracker_client),
    NewState = retry_down(State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
print_torrent_info(Torrent) ->
    print_announce(Torrent#torrent.announce),
    print_announce_lists(Torrent#torrent.announce_list),
    print_creation_date(Torrent#torrent.creation_date),
    print_comment(Torrent#torrent.comment),
    print_created_by(Torrent#torrent.created_by),
    print_encoding(Torrent#torrent.encoding),
    print_info(Torrent#torrent.info),

    io:format("~n").

print_announce(Announce) when is_list(Announce) ->
    io:format("announce: ~s~n", [Announce]);
print_announce(_) ->
    ok.

print_announce_lists(List) ->
    io:format("announce-list:~n"),
    print_announce_lists(List, 1).
print_announce_lists([H | T], N) ->
    io:format("    ~p: ", [N]),
    print_announce_list(H),
    print_announce_lists(T, N + 1);
print_announce_lists([], _) ->
    ok.

print_announce_list([H]) when is_list(H)->
    io:format("~s~n", [H]);
print_announce_list([H | T]) when is_list(H) ->
    io:format("~s, ", [H]),
    print_announce_list(T);
print_announce_list([]) ->
    io:format("~n").

print_creation_date(Date) ->
    io:format("creation date: ~p~n", [Date]).

print_comment(Comment) when is_binary(Comment) ->
    io:format("comment: ~s~n", [Comment]);
print_comment(_) ->
    ok.

print_created_by(Creator) when is_binary(Creator) ->
    io:format("created by: ~s~n", [Creator]);
print_created_by(_) ->
    ok.

print_encoding(Encoding) when is_binary(Encoding) ->
    io:format("encoding: ~s~n", [Encoding]);
print_encoding(_) ->
    ok.

print_info(Info) ->
    io:format("info:~n"),
    print_piece_length(Info#torrent_info.piece_length),
    print_pieces(Info#torrent_info.pieces),
    print_private(Info#torrent_info.private),
    print_name(Info#torrent_info.name),
    print_length(Info#torrent_info.length),
    print_md5sum(Info#torrent_info.md5sum),
    print_files(Info#torrent_info.files).

print_piece_length(Length) when is_integer(Length) ->
    io:format("    piece length: ~p~n", [Length]);
print_piece_length(_) ->
    ok.

print_pieces(Pieces) ->
    io:format("    pieces: ~s ~s ~s ~s ...~n",
              [ngbt_lib:bin_to_hexstr(binary:part(Pieces, { 0, 4})),
               ngbt_lib:bin_to_hexstr(binary:part(Pieces, { 4, 4})),
               ngbt_lib:bin_to_hexstr(binary:part(Pieces, { 8, 4})),
               ngbt_lib:bin_to_hexstr(binary:part(Pieces, {12, 4}))]).

print_private(IsPrivate) when is_integer(IsPrivate) ->
    io:format("    private: ~p~n", [IsPrivate]);
print_private(_) ->
    ok.

print_name(Name) when is_binary(Name) ->
    io:format("    name: ~s~n", [Name]);
print_name(_) ->
    ok.

print_length(Length) when is_integer(Length) ->
    io:format("    length: ~p~n", [Length]);
print_length(_) ->
    ok.

print_md5sum(MD5) when is_binary(MD5) ->
    io:format("    md5sum: ~p~n", [MD5]);
print_md5sum(_) ->
    ok.

print_files(Files) when Files =:= undefined->
    ok;
print_files(Files) ->
    io:format("    files:~n"),
    print_file(Files, 1).

print_file([H | T], N) ->
    io:format("        ~p:~n", [N]),
    print_path_in_files(H#torrent_file.path),
    print_length_in_files(H#torrent_file.length),
    print_md5sum_in_files(H#torrent_file.md5sum),
    print_file(T, N + 1);
print_file([], _) ->
    ok.

print_path_in_files(Path) ->
    io:format("            path: "),
    print_path_in_files2(Path).
print_path_in_files2([H]) when is_binary(H) ->
    io:format("~s~n", [H]);
print_path_in_files2([H | T]) when is_binary(H) ->
    io:format("~s/", [H]),
    print_path_in_files(T);
print_path_in_files2([]) ->
    io:format("~n").

print_length_in_files(Length) when is_integer(Length) ->
    io:format("            length: ~p~n", [Length]);
print_length_in_files(_) ->
    ok.

print_md5sum_in_files(MD5) when is_binary(MD5) ->
    io:format("            md5sum: ~p~n", [MD5]);
print_md5sum_in_files(_) ->
    ok.

start_down(State) when length(State#state.trackers) > 0 ->
    {ok, PeerID}  = ngbt_pref:get_peer_id(State#state.pid_pref),
    {ok, Port}    = ngbt_pref:get_port(State#state.pid_pref),

    [Tracker | _] = State#state.trackers,

    case ngbt_tracker_client:start_link(Tracker,
                                        State#state.info_hash,
                                        PeerID,
                                        Port,
                                        0) of
        {ok, PID} ->
            case ngbt_tracker_client:get_peers(PID) of
                {ok, _} ->
                    {ok, PID}
            end
    end;
start_down(State) ->
    {ok, State#state.pid_tracker_client}.

stop_down(State) when is_pid(State#state.pid_tracker_client) ->
    PID = State#state.pid_tracker_client,

    ngbt_tracker_client:set_event(PID, stopped),
    ngbt_tracker_client:get_peers(PID),
    ngbt_tracker_client:stop(PID);
stop_down(_) ->
    ok.

make_trackers(Announce, [H | T], Trackers) ->
    make_trackers(Announce, T, make_trackers2(Announce, H, Trackers));
make_trackers(_, [], Trackers) ->
    lists:reverse(Trackers).

make_trackers2(Announce, [H | T], Trackers) when H =/= Announce->
    make_trackers2(Announce, T, [H | Trackers]);
make_trackers2(Announce, [_ | T], Trackers) ->
    make_trackers2(Announce, T, Trackers);
make_trackers2(_, [], Trackers) ->
    Trackers.

retry_down(State) when length(State#state.trackers) > 0 ->
    [H | T] = State#state.trackers,

    Trackers = lists:reverse([H | lists:reverse(T)]),

    case start_down(State#state{trackers = Trackers,
                                pid_tracker_client = undefined}) of
        {ok, PID} ->
            State#state{trackers = Trackers, pid_tracker_client = PID}
    end;
retry_down(State) ->
    State.

tracker_res_handler(Res, State) ->
    tracker_res_failure(Res, State).

tracker_res_failure(Res, State) ->
    case dict:find(<<"failure reason">>, Res) of
        {ok, Value} when is_binary(Value) ->
            io:format("    failure reason = ~s~n", [Value]),

            ngbt_tracker_client:stop(State#state.pid_tracker_client),
            retry_down(State);
        _ ->
            tracker_res_warning(Res, State)
    end.

tracker_res_warning(Res, State) ->
    case dict:find(<<"warning massage">>, Res) of
        {ok, Value} when is_binary(Value) ->
            io:format("    warning message = ~s~n", [Value]);
        _ ->
            ok
    end,

    tracker_res_interval(Res, State).

tracker_res_interval(Res, State) ->
    case dict:find(<<"interval">>, Res) of
        {ok, Value} when is_integer(Value) ->
            io:format("    interval = ~p~n", [Value]),
            tracker_res_min_interval(Res, State#state{interval = Value});
        _ ->
            tracker_res_min_interval(Res, State)
    end.

tracker_res_min_interval(Res, State) ->
    case dict:find(<<"min interval">>, Res) of
        {ok, Value} when is_integer(Value) ->
            io:format("    min interval = ~p~n", [Value]),
            tracker_res_tracker_id(Res, State#state{min_interval = Value});
        _ ->
            tracker_res_tracker_id(Res, State)
    end.

tracker_res_tracker_id(Res, State) ->
    case dict:find(<<"tracker id">>, Res) of
        {ok, Value} when is_binary(Value) ->
            io:format("    tracker id = ~p~n", [Value]),
            ngbt_tracker_client:set_tracker_id(State#state.pid_tracker_client,
                                               Value);
        _ ->
            ok
    end,

    tracker_res_complete(Res, State).

tracker_res_complete(Res, State) ->
    case dict:find(<<"complete">>, Res) of
        {ok, Value} when is_integer(Value) ->
            io:format("    complete = ~p~n", [Value]),
            tracker_res_incomplete(Res, State#state{complete = Value});
        _ ->
            tracker_res_incomplete(Res, State)
    end.

tracker_res_incomplete(Res, State) ->
    case dict:find(<<"incomplete">>, Res) of
        {ok, Value} when is_integer(Value) ->
            io:format("    incomplete = ~p~n", [Value]),
            tracker_res_peers(Res, State#state{incomplete = Value});
        _ ->
            tracker_res_peers(Res, State)
    end.

tracker_res_peers(Res, State) ->
    case dict:find(<<"peers">>, Res) of
        {ok, Value} when is_binary(Value) ->
            tracker_res_peer(Value, State);
        _ ->
            State
    end.

tracker_res_peer(<<IP1:8, IP2:8, IP3:8, IP4:8, Port:16/integer-big-unsigned,
                   Peers/binary>>, State) ->
    io:format("    IP = ~p, Port = ~p~n", [{IP1, IP2, IP3, IP4}, Port]),

    add_peer(State#state.peers, {IP1, IP2, IP3, IP4}, Port),

    tracker_res_peer(Peers, State);
tracker_res_peer(_, State) ->
    State.

add_peer(TID, IP, Port) ->
    case ets:member(TID, {IP, Port}) of
        true ->
            ok;
        false ->
            %% TODO: contact to peers
            
            ets:insert(TID, {{IP, Port}, undefined, undefined})
    end.
