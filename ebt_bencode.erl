-module(ebt_bencode).

%% this code is presented by
%% http://wiki.theory.org/Decoding_encoding_bencoded_data_with_erlang

%% API
-export([decode/1, encode/1]).

%% You are able to choose the dict implementation
-define(DICT, dict).
%%-define(DICT, orddict).

%%====================================================================
%% API
%%====================================================================
decode(Data) ->
    case catch dec(Data, 0) of
	{'EXIT', _, _} ->
	    {error, unparsed};
	{Res, _, _} ->
	    {ok, Res}
    end.

encode(Struct) ->
    iolist_to_binary(enc(Struct)).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Decoding
%%--------------------------------------------------------------------
dec(<<$l, Tail/binary>>, N) ->
    dec_list(Tail, [], N + 1);
dec(<<$d, Tail/binary>>, N) ->
    dec_dict(Tail, ?DICT:new(), N + 1);
dec(<<$i, Tail/binary>>, N) ->
    dec_int(Tail, [], N + 1);
dec(Data, N) ->
    dec_string(Data, [], N).

dec_int(<<$e, Tail/binary>>, Acc, N) ->
    {list_to_integer(lists:reverse(Acc)), Tail, N + 1};
dec_int(<<X, Tail/binary>>, Acc, N) ->
    dec_int(Tail, [X|Acc], N + 1).

dec_string(<<$:, Tail/binary>>, Acc, N) ->
    Int = list_to_integer(lists:reverse(Acc)),
    <<Str:Int/binary, Rest/binary>> = Tail,
    {Str, Rest, N + Int + 1};
dec_string(<<X, Tail/binary>>, Acc, N) ->
    dec_string(Tail, [X|Acc], N + 1).

dec_list(<<$e, Tail/binary>>, Acc, N) ->
    {{list, lists:reverse(Acc)}, Tail, N + 1};
dec_list(Data, Acc, N) ->
    {Res, Tail, N1} = dec(Data, N),
    dec_list(Tail, [Res|Acc], N1).

dec_dict(<<$e, Tail/binary>>, Acc, N) ->
    {{dict, Acc}, Tail, N + 1};
dec_dict(Data, Acc, N) ->
    {Key, Tail1, N1} = dec(Data, N),
    {Val, Tail2, N2} = dec(Tail1, N1),

    case Key of
        <<"info">> ->
            put(info_begin, N1),
            put(info_len, N2 - N1 - 1);
        _ ->
            ok
    end,
    
    dec_dict(Tail2, ?DICT:store(Key, Val, Acc), N2).

%%--------------------------------------------------------------------
%% Encoding
%%--------------------------------------------------------------------
enc(Int) when is_integer(Int) ->
    IntBin = list_to_binary(integer_to_list(Int)),
    [$i, IntBin, $e];
enc(Str) when is_list(Str) ->
    enc(list_to_binary(Str));
enc(Str) when is_binary(Str) ->
    IntBin = list_to_binary(integer_to_list(size(Str))),
    [IntBin, $:, Str];
enc({list, List}) when is_list(List) ->
    [$l, [enc(Elem) || Elem <- List], $e];
enc({dict, Dict}) ->
    Data = lists:map(
	     fun({Key, Val}) when is_list(Key) or is_binary(Key) ->
		     [enc(Key), enc(Val)]
	     end, lists:keysort(1, ?DICT:to_list(Dict))),
    [$d, Data, $e].
