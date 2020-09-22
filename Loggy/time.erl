-module(zero).
-export([zero/0, inc/2, merge/2, leq/2]).

zero() ->
    0.

inc(_Name, T) ->
    T+1.

merge(Ti, Tj) ->
    case Ti >= Tj of
        true -> Ti;
        false -> Tj
    end.
        
leq(Ti, Tj) when Ti =< Tj->
    true;
leq(_Ti, _Tj)->
    false.


