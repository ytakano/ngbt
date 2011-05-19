%%%-------------------------------------------------------------------
%%% @author ytakano <ytakanoster@gmail.com>
%%% @copyright (C) 2011, Yuuki Takano
%%% @doc
%%%
%%% @end
%%% Created : 18 May 2011 by Yuuki Takano <ytakanoster@gmail.com>
%%%-------------------------------------------------------------------
-module(ngbt_pieces).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, set_bitfield/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {num, pieces, bitfield}).

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
start_link(Num) ->
    gen_server:start_link(?MODULE, [Num], []).

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
%% Starts the server
%%
%% @spec set_bitfield(PID, Pos, Flag) -> ok
%% @end
%%--------------------------------------------------------------------
set_bitfield(PID, Pos, Flag) ->
    gen_server:cast(PID, {set_bitfield, Pos, Flag}).

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
init([Num]) ->
    {ok, #state{num = Num, pieces = init_pieces(Num), bitfield = <<0:Num>>}}.

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
handle_cast({set_bitfield, Pos, Flag}, State) ->
    BitField = set_bitfield0(State#state.bitfield, Pos, Flag),
    {noreply, State#state{bitfield = BitField}};
handle_cast(stop, State) ->
    ets:delete(State#state.pieces),
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
init_pieces(Num) ->
    init_pieces(0, Num, ets:new(pieces, [set, private])).

init_pieces(N, Num, TID) when N < Num ->
    ets:insert(TID, {N, 0}),
    init_pieces(N + 1, Num, TID);
init_pieces(_, _, TID) ->
    TID.

set_bitfield0(BitField, Pos, Flag) when Pos < bit_size(BitField) andalso
                                        Pos >= 0 ->
    TailSize = bit_size(BitField) - Pos - 1,
    <<H:Pos, _:1, T:TailSize>> = BitField,

    case Flag of
        true ->
            <<H:Pos, 1:1, T:TailSize>>;
        _ ->
            <<H:Pos, 0:1, T:TailSize>>
    end;
set_bitfield0(BitField, _, _) ->
    BitField.
