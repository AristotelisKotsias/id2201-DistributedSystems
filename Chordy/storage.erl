-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

% create a new storage
create() -> 
    [].

% add a key value pair, return updated store
add(Key, Value, Store) ->
    [{Key, Value}|Store].

% return a tuple {Key, Value} or false
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

% return a tuple {Updated, Rest} where the updated store only contains 
% the key value pairs requested and the rest are found in a list of 
% key-value pairs
split(From, To, Store) ->
    lists:partition(fun({Key, _Value}) -> key:between(Key, From, To) end, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) ->
    lists:merge(Entries, Store).