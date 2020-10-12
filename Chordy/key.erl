-module(key).
-export([generate/0, between/3]).

generate() -> 
    rand:uniform(1000000000).

between(Key, From, To) when From < To ->
    From < Key andalso Key =< To;
between(Key, From, To) when From > To ->
    Key =< To orelse From < Key;
between(_Key, Equal, Equal) ->
    true.
