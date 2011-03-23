-module(ebt_lib).

-export([bin_to_hexstr/1]).

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

