%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ngbt_files).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/1, write/4, read/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("ngbt_torrent.hrl").

-record(file, {length, md5sum, is_completed}).

-record(state, {piece_length, hashes, paths = [], files, blocks,
                pid_pieces}).

-define(BLOCK_SIZE, 16384).
-define(MAX_BLOCK_SIZE, 32768).

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
start_link(Info, PIDPieces) ->
    gen_server:start_link(?MODULE, [Info, PIDPieces], []).

%%--------------------------------------------------------------------
%% @doc
%% initialize
%%
%% @spec init_files(PID) -> ok
%% @end
%%--------------------------------------------------------------------
init_files(PID, Info) ->
    gen_server:cast(PID, {init_files, Info}).

%%--------------------------------------------------------------------
%% @doc
%% write data to files
%%
%% @spec write(PID, Index, Begin, Data) -> ok
%% @end
%%--------------------------------------------------------------------
write(PID, Index, Begin, Data) ->
    gen_server:cast(PID, {write, Index, Begin, Data}).

%%--------------------------------------------------------------------
%% @doc
%% read data from files
%%
%% @spec read(PID, Index, Begin, Size) -> ok
%% @end
%%--------------------------------------------------------------------
read(PID, Index, Begin, Size) ->
    gen_server:cast(PID, {read, Index, Begin, Size}).

%%--------------------------------------------------------------------
%% @doc
%% stop the process
%%
%% @spec stop(PID) -> ok
%% @end
%%--------------------------------------------------------------------
stop(PID) ->
    gen_server:cast(PID, stop).

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
init([Info, PIDPieces]) ->
    PID = self(),
    spawn_link(fun() -> init_files(PID, Info) end),
    {ok, #state{blocks = ets:new(blocks, [set, private, {keypos, 1}]),
                pid_pieces = PIDPieces}}.

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
handle_call({read, Index, Begin, Len}, _From, State)
  when Index < byte_size(State#state.hashes) / 20 andalso
       Len =< State#state.piece_length - Begin ->

    Reply = case read_from_files(Index, Begin, Len, State#state.piece_length,
                                 State#state.blocks) of
                {ok, Data} ->
                    {ok, Data};
                _ ->
                    false
            end,

    %% TODO: reply by MPI

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
handle_cast({init_files, Info}, State)
  when Info#torrent_info.piece_length < ?BLOCK_SIZE orelse
       Info#torrent_info.piece_length rem ?BLOCK_SIZE > 0 ->
    %% TODO: handle error
    %% invalid torrent file
    {noreply, State};
handle_cast({init_files, Info}, State) ->
    {Paths, Files} = gen_paths_and_files(Info),

    io:format("Paths = ~p~n", [Paths]),

    init_blocks(Paths, Files, State#state.blocks, 0, 0, 0),

    NewFiles = check_files(Paths,
                           Info#torrent_info.pieces,
                           Info#torrent_info.piece_length,
                           Files,
                           State#state.blocks,
                           State#state.pid_pieces),

    {noreply, State#state{piece_length = Info#torrent_info.piece_length,
                          hashes       = Info#torrent_info.pieces,
                          paths        = Paths,
                          files        = NewFiles}};
handle_cast({write, Index, Begin, Data}, State)
  when Begin rem State#state.piece_length == 0 andalso
       Index < byte_size(State#state.hashes) / 20 andalso
       byte_size(Data) =< ?BLOCK_SIZE ->

    write_to_files(Index, Begin, Data,
                   binary_part(State#state.hashes, Index, 20),
                   State#state.piece_length, State#state.blocks),

    {noreply, State};
handle_cast(stop, State) ->
    ets:delete(State#state.blocks),
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
gen_paths_and_files(Info) when not is_list(Info#torrent_info.files) ->
    Path = binary_to_list(Info#torrent_info.name),

    File = #file{length = Info#torrent_info.length,
                 md5sum = Info#torrent_info.md5sum},

    Files = dict:append(Path, File, dict:new()),

    {[Path], Files};
gen_paths_and_files(Info) when is_binary(Info#torrent_info.name) andalso
                               is_list(Info#torrent_info.files) ->
    gen_paths_and_files(binary_to_list(Info#torrent_info.name),
                        Info#torrent_info.files, [], dict:new());
gen_paths_and_files(_) ->
    [].

gen_paths_and_files(Dir, [H | T], Paths, Files) ->
    P = [binary_to_list(N) || N <- H#torrent_file.path, is_binary(N)],
    Path = filename:join([Dir | P]),

    File = #file{length = H#torrent_file.length,
                 md5sum = H#torrent_file.md5sum},

    NewFiles = dict:append(Path, File, Files),

    gen_paths_and_files(Dir, T, [Path | Paths], NewFiles);
gen_paths_and_files(_, [], Paths, Files) ->
    {lists:reverse(Paths), Files}.

check_files(Paths, Hashes, PieceLen, Files, Blocks, PIDPieces) ->
    check_files(Paths, Hashes, PieceLen, 0, Files, crypto:sha_init(), 0, 0,
                Blocks, PIDPieces).

check_files([H | T], Hashes, PieceLen, Location, Files, Context, Read, Index,
            Blocks, PIDPieces) ->
    io:format("Path     = ~s:~n", [H]),
    io:format("Read     = ~p~n", [Read]),
    io:format("PieceLen = ~p~n", [PieceLen]),

    case dict:find(H, Files) of
        {ok, [File | _]} ->
            case file:open(H, [read, binary]) of
                {ok, IoDevice} ->
                    {NewHashes,
                     NewLocation,
                     NewContext,
                     NewRead,
                     NewIndex} = check_file(IoDevice, Hashes, PieceLen,
                                            Location, File#file.length,
                                            Context, Read, Index, Blocks,
                                            PIDPieces),

                    check_files(T, NewHashes, PieceLen, NewLocation, Files,
                                NewContext, NewRead, NewIndex, Blocks,
                                PIDPieces);
                _ ->
                    if
                        File#file.length =< Location ->
                            TailLen   = File#file.length - Location,
                            Num       = ngbt_lib:ceil(TailLen / PieceLen),
                            HashBytes = 20 * Num,

                            case Hashes of
                                <<_:HashBytes/binary, NewHashes/binary>> ->
                                    check_files(T, NewHashes, PieceLen,
                                                PieceLen * Num - TailLen,
                                                File#file.length,
                                                crypto:sha_init(), 0,
                                                Index + Num, Blocks,
                                                PIDPieces);
                                _ ->
                                    %% TODO: handle error
                                    %% invalid torrent file
                                    ok
                            end;
                        true ->
                            check_files(T, Hashes, PieceLen,
                                        Location - File#file.length,
                                        File#file.length, crypto:sha_init(), 0,
                                        Index, Blocks, PIDPieces)
                    end
            end;
        _ ->
            ok
    end;
check_files([], <<Hash:20/binary>>, PieceLen, _, _, Context, _, Index,
            Blocks, PIDPieces) ->
    HashPiece = crypto:sha_final(Context),
    if
        Hash =:= HashPiece ->
            set_index_state(Index, completed, PieceLen, Blocks, PIDPieces);
        true ->
            ok
    end;
check_files([], _, _, _, _, _, _, _, _, _) ->
    ok.

check_file(IoDevice, Hashes = <<Hash:20/binary, NewHashes/binary>>, PieceLen,
           Location, FileSize, Context, Read, Index, Blocks, PIDPieces) ->
    case file:pread(IoDevice, Location, PieceLen - Read) of
        {ok, Data} ->
            if
                byte_size(Data) + Read =:= PieceLen ->
                    HashPiece = crypto:sha_final(crypto:sha_update(Context,
                                                                   Data)),

                    if
                        Hash =:= HashPiece ->
                            set_index_state(Index, completed, PieceLen, Blocks,
                                            PIDPieces);
                        true ->
                            ok
                    end,

                    check_file(IoDevice, NewHashes, PieceLen,
                               Location + PieceLen - Read, FileSize,
                               crypto:sha_init(), 0, Index + 1, Blocks,
                               PIDPieces);
                true ->
                    if
                        Location + byte_size(Data) =:= FileSize ->
                            {Hashes, 0, crypto:sha_update(Context, Data),
                             byte_size(Data) + Read, Index};
                        true ->
                            skip_bytes(Hashes, PieceLen, Location, FileSize,
                                       Index)
                    end
            end;
        eof ->
            {Hashes, 0, crypto:sha_init(), 0, Index};
        _ ->
            skip_bytes(Hashes, PieceLen, Location, FileSize, Index)
    end;
check_file(_, Hashes, PieceLen, Location, FileSize, _, _, Index, _, _) ->
    skip_bytes(Hashes, PieceLen, Location, FileSize, Index).

skip_bytes(Hashes, PieceLen, Location, FileSize, Index) ->
    Remain = FileSize - Location,
    Num    = ngbt_lib:ceil(Remain / PieceLen),
    Bytes  = 20 * Num,

    case Hashes of
        <<_:Bytes/binary, NewHashes/binary>> ->
            {NewHashes, Num * PieceLen - Remain, crypto:sha_init(), 0,
             Index + Num};
        _ ->
            %% TODO: handle error
            %% invalid torrent file
            {<<>>, Num * PieceLen - Remain, crypto:sha_init(), 0, Index}
    end.

init_blocks(Paths = [H | T], Files, Blocks, TotalLen, Block, Pos) ->
    case dict:find(H, Files) of
        {ok, [File | _]} ->
            if
                TotalLen + File#file.length - Pos < ?BLOCK_SIZE ->
                    add_index(Block, H, Pos, File#file.length - Pos,
                              Blocks),
                    init_blocks(T, Files, Blocks,
                                TotalLen + File#file.length - Pos, Block, 0);
                TotalLen + File#file.length - Pos == ?BLOCK_SIZE ->
                    add_index(Block, H, Pos, File#file.length - Pos,
                              Blocks),
                    init_blocks(T, Files, Blocks, 0, Block + 1, 0);
                true ->
                    add_index(Block, H, Pos, ?BLOCK_SIZE - TotalLen,
                              Blocks),
                    init_blocks(Paths, Files, Blocks, 0, Block + 1,
                                ?BLOCK_SIZE - TotalLen + Pos)
            end;
        _ ->
            %% TODO: handle error
            %% invalid torrent file
            ok
    end;
init_blocks([], _, _, _, _, _) ->
    ok.

add_index(Block, Path, Pos, Len, TID) ->
    case ets:lookup(TID, Block) of
        [{Block, State, Paths}] ->
            L = [{Path, Pos, Len}, lists:reverse(Paths)],
            NewPaths = lists:reverse(L),
            ets:insert(TID, {Block, State, NewPaths});
        [] ->
            ets:insert(TID, {Block, incompleted, [{Path, Pos, Len}]})
    end.

set_index_state(Index, State, PieceLen, TID, PIDPieces) ->
    N = trunc(PieceLen / ?BLOCK_SIZE),
    Block = Index * N,
    set_block_state(Block, Block + N, State, TID),

    case State of
        completed ->
            ngbt_pieces:set_bitfield(PIDPieces, Index, true);
        _ ->
            ok
    end.

set_block_state(Block, State, TID) ->
    case ets:lookup(TID, Block) of
        [{Block, _, Paths}] ->
            ets:insert(TID, {Block, State, Paths});
        [] ->
            ok
    end.

set_block_state(Begin, End, State, TID) when Begin < End -> 
    set_block_state(Begin, State, TID),
    set_block_state(Begin + 1, End, State, TID);
set_block_state(_, _, _, _) ->
    ok.

write_to_files(Index, Begin, Data, Hash, PieceLen, TID) ->
    N = PieceLen / ?BLOCK_SIZE,
    Block = trunc(Index * N + Begin / ?BLOCK_SIZE),

    case ets:lookup(TID, Block) of
        [{Block, incompleted, Paths}] ->
            Total = lists:sum([Len || {_, _, Len} <- Paths]),

            if
                Total =:= byte_size(Data) ->
                    case write_to_files(Paths, Data) of
                        ok ->
                            ets:insert(TID, {Block, downloaded, Paths}),
                            check_piece(Index, trunc(N), Hash, TID);
                        _ ->
                            ok
                    end;
                true ->
                    ok
            end;
        _ ->
            ok
    end.

write_to_files([{Path, Pos, Len} | T], Data) ->
    <<DataW:Len/binary, Rem/binary>> = Data,

    case file:open(Path, [append, raw, binary]) of
        {ok, IoDev} ->
            file:pwrite(IoDev, Pos, DataW),
            file:close(IoDev);
        _ ->
            %% TODO: handle error
            %% cannot open file
            error
    end,

    write_to_files(T, Rem);
write_to_files([], _) ->
    ok.

check_piece(Index, NumBlock, Hash, TID) ->
    Begin = Index * NumBlock,
    End   = Begin + NumBlock,

    case check_piece2(Begin, End, TID) of
        true ->
            check_piece3(Begin, End, Hash, crypto:sha_init(), TID);
        _ ->
            ok
    end.

check_piece2(Begin, End, TID) when Begin < End ->
    case ets:lookup(TID, Begin) of
        [{Begin, downloaded, _}] ->
            check_piece2(Begin + 1, End, TID);
        _ ->
            false
    end;
check_piece2(_, _, _) ->
    true.

check_piece3(Begin, End, Hash, Context, TID) when Begin < End ->
    case ets:lookup(TID, Begin) of
        [{Begin, downloaded, Paths}] ->
            NewContext = update_context(Paths, Context),
            check_piece3(Begin + 1, End, Hash, NewContext, TID);
        [] ->
            false
    end;
check_piece3(Begin, End, Hash, Context, TID) ->
    case crypto:sha_final(Context) of
        Hash ->
            %% piece was just downloaded
            set_block_state(Begin, End, completed, TID);
        _ ->
            %% piece downloading must have been failed
            set_block_state(Begin, End, incompleted, TID)
    end.

update_context([{Path, Pos, Len} | T], Context) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, IoDev} ->
            Data = file:pread(IoDev, Pos, Len),
            file:close(IoDev),
            update_context(T, crypto:sha_update(Context, Data));
        _ ->
            Context
    end;
update_context([], Context) ->
    Context.

read_from_files(Index, Begin, Len, PieceLen, TID) ->
    N = PieceLen / ?BLOCK_SIZE,
    Block = Index * N + Begin / ?BLOCK_SIZE,

    read_blocks(Block, Begin rem ?BLOCK_SIZE, Len, TID, <<>>).

read_blocks(Block, Begin, Len, TID, Data) when Len > 0 ->
    case ets:lookup(TID, Block) of
        [{Block, downloaded, Paths}] ->
            case read_files(Paths, Begin, Len, <<>>) of
                {ok, Bin} ->
                    read_blocks(Block + 1, 0, Len - byte_size(Bin), TID,
                                <<Data/binary, Bin/binary>>);
                _ ->
                    false
            end;
        _ ->
            false
    end;
read_blocks(_, _, _, _, Data) ->
    Data.

read_files([{_, _, PathLen} | T], Begin, Len, Data) when PathLen < Begin ->
    read_files(T, Begin - PathLen, Len, Data);
read_files([{Path, Pos, PathLen} | T], Begin, Len, Data) when Len > 0->
    PLen = PathLen - Begin,

    ReadLen = if
                  Len > PLen ->
                      PLen;
                  true ->
                      Len
              end,

    case file:open(Path, [append, raw, binary]) of
        {ok, IoDev} ->
            Ret = file:pread(IoDev, Pos + Begin, ReadLen),
            file:close(IoDev),

            case Ret of
                {ok, ReadData} ->
                    if
                        ReadLen =:= byte_size(ReadData) ->
                            read_files(T, 0, Len - ReadLen,
                                       <<Data/binary, ReadData/binary>>);
                        true ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end;
read_files(_, _, Len, Data) when Len =:= 0 ->
    {ok, Data};
read_files([], _, _, Data) ->
    {ok, Data};
read_files(_, _, _, _) ->
    false.
