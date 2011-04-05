-module(ebt_lib).

-export([bin_to_hexstr/1, escape_uri/1, sleep/1]).

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

escape_uri(Bin) when is_binary(Bin) ->
    escape_uri(binary_to_list(Bin));
escape_uri([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|escape_uri(T)];
        H >= $A, $Z >= H ->
            [H|escape_uri(T)];
        H >= $0, $9 >= H ->
            [H|escape_uri(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|escape_uri(T)];
        true ->
            case integer_to_list(H, 16) of
                [X, Y] ->
                    [$%, X, Y | escape_uri(T)];
                [X] ->
                    [$%, $0, X | escape_uri(T)]
            end
    end;
escape_uri([]) ->
[]. 

sleep(MSec) ->
    receive
    after MSec ->
            ok
    end.
