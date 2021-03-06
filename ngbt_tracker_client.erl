%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ngbt_tracker_client).

-behaviour(gen_server).

%% API
-export([start_link/5, stop/1]).
-export([get_peers/1, set_event/2, set_tracker_id/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {tracker,
                info_hash,
                peer_id,
                port,
                uploaded   = 0,
                downloaded = 0,
                left,
                compact    = 1,
                event,     %% optional
                ip,        %% optional
                numwant,   %% optional
                key,       %% optional
                trackerid  %% optional
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Tracker, InfoHash, PeerID, Port, Left) ->
%%           {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Tracker, InfoHash, PeerID, Port, Left) ->
    gen_server:start_link(?MODULE, [Tracker, InfoHash, PeerID, Port, Left], []).

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
%% get peers from tracker
%%
%% @spec get_peers(PID) -> {ok, Ref}
%% @end
%%--------------------------------------------------------------------
get_peers(PID) ->
    Ref = make_ref(),
    case gen_server:call(PID, {get_peers, Ref}) of
        ok ->
            {ok, Ref};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% set event
%%
%% @spec set_event(PID, Event) -> ok
%% @end
%%--------------------------------------------------------------------
set_event(PID, Event) ->
    gen_server:cast(PID, {set_event, Event}).

%%--------------------------------------------------------------------
%% @doc
%% set event
%%
%% @spec set_tracker_id(PID, ID) -> ok
%% @end
%%--------------------------------------------------------------------
set_tracker_id(PID, ID) ->
    gen_server:cast(PID, {set_tracker_id, ID}).

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
init([Tracker, InfoHash, PeerID, Port, Left]) ->
    {ok, #state{tracker   = Tracker,
                info_hash = InfoHash,
                peer_id   = PeerID,
                port      = Port,
                left      = Left}}.

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
handle_call({get_peers, Ref}, {FromPID, _}, State) ->
    PID = self(),
    spawn_link(fun() -> peers_from_tracker(Ref, FromPID, PID, State) end),
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
handle_cast({set_tracker_id, ID}, State) when is_binary(ID) ->
    {noreply, State#state{trackerid = binary_to_list(ID)}};
handle_cast({set_event, Event}, State) ->
    {noreply, State#state{event = Event}};
handle_cast(stop, State) ->
    case State#state.event of
        stopped ->
            ok;
        undefined ->
            ok;
        _ ->
            peers_from_tracker(make_ref(), self(), self(),
                               State#state{event = stopped})
    end,
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
gen_info_hash(InfoHash) ->
    "info_hash=" ++ ngbt_lib:escape_uri(InfoHash).

gen_peer_id(PeerID) ->
    "&peer_id=" ++ ngbt_lib:escape_uri(PeerID).

gen_port(Port) ->
    "&port=" ++ integer_to_list(Port).

gen_uploaded(Bytes) ->
    "&uploaded=" ++ integer_to_list(Bytes).

gen_downloaded(Bytes) ->
    "&downloaded=" ++ integer_to_list(Bytes).

gen_left(Bytes) ->
    "&left=" ++ integer_to_list(Bytes).

gen_compact(Compact) ->
    "&compact=" ++ integer_to_list(Compact).

gen_event(Event) when Event =:= undefined ->
    "&event=started";
gen_event(Event) when Event =:= started ->
    "";
gen_event(Event) when Event =:= stopped ->
    "&event=stopped";
gen_event(Event) when Event =:= completed ->
    "&event=completed";
gen_event(_) ->
    "".

gen_ip(IP) when is_list(IP) ->
    "&ip=" ++ IP;
gen_ip(_) ->
    "".

gen_numwant(Num) when is_integer(Num) ->
    "&numwant=" ++ integer_to_list(Num);
gen_numwant(_) ->
    "".

gen_key(Key) when is_list(Key) ->
    "&key=" ++ ngbt_lib:escape_uri(Key);
gen_key(_) ->
    "".

gen_trackerid(TrackerID) when is_list(TrackerID) ->
    "&trackerid=" ++ ngbt_lib:escape_uri(TrackerID);
gen_trackerid(_) ->
    "".

gen_url(State) ->
    lists:flatten([State#state.tracker, "?",
                   gen_info_hash(State#state.info_hash),
                   gen_peer_id(State#state.peer_id),
                   gen_port(State#state.port),
                   gen_uploaded(State#state.uploaded),
                   gen_downloaded(State#state.downloaded),
                   gen_left(State#state.left),
                   gen_compact(State#state.compact),
                   gen_event(State#state.event),
                   gen_ip(State#state.ip),
                   gen_numwant(State#state.numwant),
                   gen_key(State#state.key),
                   gen_trackerid(State#state.trackerid)]).

peers_from_tracker(Ref, From, PID, State) ->
    URL = gen_url(State),

    io:format("URL = ~s~n", [URL]),

    case httpc:request(URL) of
        {ok, {Status, _, Body}} ->
            send_result(Ref, From, Status, Body);
        _ ->
            send_result(Ref, From, error, "error")
    end,

    case State#state.event of
        undefined ->
            catch set_event(PID, started);
        _ ->
            ok
    end.

send_result(Ref, From, {_, 200, "OK"}, Body) ->
    catch From ! {peers, Ref, ok, list_to_binary(Body)};
send_result(Ref, From, _, Body) ->
    catch From ! {peers, Ref, error, list_to_binary(Body)}.
