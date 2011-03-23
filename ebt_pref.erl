%%%-------------------------------------------------------------------
%%% @author Yuuki Takano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ebt_pref).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([init/0, get_peer_id/0, print/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {peer_id}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Initialize
%%
%% @spec init_pref() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
init() ->
    gen_server:call(?SERVER, init).

%%--------------------------------------------------------------------
%% @doc
%% get the id of peer
%%
%% @spec get_peer_id() -> {ok, ID}
%% @end
%%--------------------------------------------------------------------
get_peer_id() ->
    gen_server:call(?SERVER, get_peer_id).

%%--------------------------------------------------------------------
%% @doc
%% Initialize
%%
%% @spec print() -> ok
%% @end
%%--------------------------------------------------------------------
print() ->
    gen_server:call(?SERVER, print).

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
handle_call(init, _From, _State) ->
    State = initialize(),
    Reply = ok,
    {reply, Reply, State};
handle_call(get_peer_id, _From, State) ->
    Reply = {ok, State#state.peer_id},
    {reply, Reply, State};    
handle_call(print, _From, State) ->
    print_pref(State),
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
initialize() ->
    PEER_ID = gen_peer_id(),
    #state{peer_id = PEER_ID}.

gen_peer_id() ->
    RND = crypto:rand_bytes(12),
    binary_to_list(<<<<"-ET0001-">>/binary, RND/binary>>).

print_pref(State) ->
    case is_list(State#state.peer_id) of
        true ->
            io:format("peer_id: "),
            lists:map(fun(X) -> io:format("~2.16.0B", [X]) end,
                      State#state.peer_id),
            io:format("~n");
        _ ->
            io:format("peer_id: unknown~n")
    end.
