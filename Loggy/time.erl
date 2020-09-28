-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_Name, T) ->
    T+1.

merge(Ti, Tj) ->
    case Ti >= Tj of
        true -> Ti;
        false -> Tj
    end.
        
leq(Ti, Tj) when Ti =< Tj ->
    true;
leq(Ti, Tj) when Ti > Tj ->
    false.

%return a clock that can keep track of the nodes
clock(Nodes)->
    lists:map(fun(Node)->{Node, zero()} end, Nodes).

%return a clock that has been updated given that we have received a
%log message from a node at a given time
update(Node, Time, Clock) ->
    lists:keysort(2, lists:keyreplace(Node, 1, Clock, {Node, Time})).

% is it safe to log an event that happened at a given time, true or false
safe(Time, [{_Node, T}|_Tail]) ->
    leq(Time, T).


