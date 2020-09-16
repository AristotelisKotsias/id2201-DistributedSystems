-module(intf).
-compile(export_all).

new() ->
    [].

%adds a new entry to the set and return the new set of interfaces
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

%removes an entry given a name of an interface, return a new set of interfaces
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

%finds the process identifier given a name, return {ok, Pid} if found 
%otherwise notfound
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, _, Pid} -> 
            {ok, Pid};
        false ->
            notfound
    end.

%finds the reference given a name and return {ok,Ref} or notfound
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, Ref, _} -> 
            {ok, Ref};
        false ->
            notfound
    end.

%finds the name of an entry given a reference and return {ok, Name} or notfound
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, _, _} -> 
            {ok, Name};
        false ->
            notfound
    end.

%returns a list with all names
list(Intf) ->
    lists:map(fun({Name, _, _}) ->  Name end, Intf).

%sends the message to all interface processes
broadcast(Message, Intf) ->
    lists:map(fun({_, _, Pid}) -> Pid ! Message end, Intf).
