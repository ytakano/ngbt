%%%-------------------------------------------------------------------
%%% @author ytakano <ytakano@dp117-119.jaist.ac.jp>
%%% @copyright (C) 2011, ytakano
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2011 by ytakano <ytakano@dp117-119.jaist.ac.jp>
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

-record(file, {length, md5sum, is_completed, device}).

-record(state, {hashes, paths = [], files}).

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
    {Paths, Files} = gen_paths_and_files(Info),

    io:format("Paths = ~p~n", [Paths]),

    {ok, #state{hashes = Info#torrent_info.pieces,
                paths  = Paths,
                files  = Files}}.

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
