%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ebt_files).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("ebt_torrent.hrl").

-record(file, {length, md5sum, is_completed}).

-record(state, {piece_length, hashes, paths = [], files}).

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
start_link(Info) ->
    gen_server:start_link(?MODULE, [Info], []).

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
init([Info]) ->
    PID = self(),
    spawn_link(fun() -> init_files(PID, Info) end),
    {ok, #state{}}.

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
handle_cast({init_files, Info}, State) ->
    {Paths, Files} = gen_paths_and_files(Info),

    io:format("Paths = ~p~n", [Paths]),

    NewFiles = check_files(Paths,
                           Info#torrent_info.pieces,
                           Info#torrent_info.piece_length,
                           Files),

    {stop, normal, State#state{piece_length = Info#torrent_info.piece_length,
                               hashes       = Info#torrent_info.pieces,
                               paths        = Paths,
                               files        = NewFiles}};
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
gen_paths_and_files(Info) when not is_list(Info#torrent_info.files) ->
    Path = binary_to_list(Info#torrent_info.name),

    File = #file{length = Info#torrent_info.length,
                 md5sum = Info#torrent_info.md5sum},

    Files = dict:append(Path, File, dict:new()),

    {[Path], Files};
gen_paths_and_files(Info) when is_binary(Info#torrent_info.name) and
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

check_files(Paths, Hashes, PieceLen, Files) ->
    check_files(Paths, Hashes, PieceLen, 0, Files, crypto:sha_init(), 0).

check_files([H | T], Hashes, PieceLen, Location, Files, Context, Read) ->
    io:format("Path = ~s:~n", [H]),
    io:format("Read     = ~p~n", [Read]),
    io:format("PieceLen = ~p~n", [PieceLen]),

    case dict:find(H, Files) of
        {ok, [File | _]} ->
            case file:open(H, [read, binary]) of
                {ok, IoDevice} ->
                    {NewHashes,
                     NewLocation,
                     NewContext,
                     NewRead} = check_file(IoDevice, Hashes, PieceLen,
                                           Location, File#file.length,
                                           Context, Read),

                    check_files(T, NewHashes, PieceLen, NewLocation, Files,
                                NewContext, NewRead);
                _ ->
                    if
                        File#file.length =< Location ->
                            TailLen   = File#file.length - Location,
                            Num       = ebt_lib:ceil(TailLen / PieceLen),
                            HashBytes = 20 * Num,

                            case Hashes of
                                <<_:HashBytes/binary, NewHashes/binary>> ->
                                    check_files(T, NewHashes, PieceLen,
                                                PieceLen * Num - TailLen,
                                                File#file.length,
                                                crypto:sha_init(), 0);
                                _ ->
                                    %% TODO: handle error
                                    ok
                            end;
                        true ->
                            check_files(T, Hashes, PieceLen,
                                        Location - File#file.length,
                                        File#file.length, crypto:sha_init(), 0)
                    end
            end;
        _ ->
            ok
    end;
check_files([], _, _, _, _, _, _) ->
    ok.

check_file(IoDevice, Hashes = <<Hash:20/binary, NewHashes/binary>>, PieceLen,
           Location, FileSize, Context, Read) ->
    case file:pread(IoDevice, Location, PieceLen - Read) of
        {ok, Data} ->
            if
                byte_size(Data) + Read =:= PieceLen ->
                    Digest = crypto:sha_final(crypto:sha_update(Context, Data)),

                    io:format("    Hash1 = ~p~n", [Hash]),
                    io:format("    Hash2 = ~p~n", [Digest]),

                    %% TODO: compare Hash and Digest

                    check_file(IoDevice, NewHashes, PieceLen,
                               Location + PieceLen - Read, FileSize,
                               crypto:sha_init(), 0);
                true ->
                    if
                        Location + byte_size(Data) =:= FileSize ->
                            {Hashes, 0, crypto:sha_update(Context, Data),
                             byte_size(Data) + Read};
                        true ->
                            skip_bytes(Hashes, PieceLen, Location, FileSize)
                    end
            end;
        eof ->
            {Hashes, 0, crypto:sha_init(), 0};
        _ ->
            skip_bytes(Hashes, PieceLen, Location, FileSize)
    end;
check_file(_, Hashes, PieceLen, Location, FileSize, _, _) ->
    skip_bytes(Hashes, PieceLen, Location, FileSize).

skip_bytes(Hashes, PieceLen, Location, FileSize) ->
    Remain = FileSize - Location,
    Num    = ebt_lib:ceil(Remain / PieceLen),
    Bytes  = 20 * Num,

    case Hashes of
        <<_:Bytes/binary, NewHashes/binary>> ->
            {NewHashes, Num * PieceLen - Remain, crypto:sha_init(), 0};
        _ ->
            %% TODO: handle error
            {<<>>, Num * PieceLen - Remain, crypto:sha_init(), 0}
    end.
