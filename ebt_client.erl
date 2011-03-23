%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ebt_client).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).
-export([read_torrent/2, print_torrent/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("ebt_torrent.hrl").

-record(state, {torrent}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
init([]) ->
    {ok, #state{torrent = #torrent{}}}.

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
    {Reply, Torrent} = case ebt_readtorrent:read(File) of
                           {ok, Data} ->
                               {ok, Data};
                           {error, Reason} ->
                               {{error, Reason}, State#state.torrent}
                       end,
    {reply, Reply, State#state{torrent = Torrent}};
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
handle_cast(stop, State) ->
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
    print_info(Torrent#torrent.info).

print_announce(Announce) when is_binary(Announce) ->
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

print_announce_list([H]) when is_binary(H)->
    io:format("~s~n", [H]);
print_announce_list([H | T]) when is_binary(H) ->
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
              [ebt_lib:bin_to_hexstr(binary:part(Pieces, { 0, 4})),
               ebt_lib:bin_to_hexstr(binary:part(Pieces, { 4, 4})),
               ebt_lib:bin_to_hexstr(binary:part(Pieces, { 8, 4})),
               ebt_lib:bin_to_hexstr(binary:part(Pieces, {12, 4}))]).

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
