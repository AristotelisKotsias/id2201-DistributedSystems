-module(dijkstra).
-compile(export_all).

%returns the length of the shortest path to the node or 0 if the node is not found
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, Shortest, _} -> 
            Shortest;
        false ->
            0
    end.

%replaces the entry for Node in Sorted with a new entry having a new length N 
%and Gateway. The entry is represented by a tuple i.e. {berlin, 2, paris}. 
%Are the entries unique?
replace(Node, N, Gateway, Sorted) ->
    lists:keysort(2, [{Node, N, Gateway} | lists:keydelete(Node, 1, Sorted)]).

%updates the list Sorted given the information that Node can be reached in N 
%hops using Gateway. If no entry is found then no new entry is added. Only if 
%we have a better (shorter) path should we replace the existing entry
%Sorted list example -> [{berlin, 2, paris}, {athens, 5, madrid}]
update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) =< N of
        true ->
            Sorted;
        false ->
            replace(Node, N, Gateway, Sorted)
    end.

%iterate(Sorted, Map, Table)
iterate([], _Map, Table) ->
    Table;
iterate([{_Node, inf, _Gateway} | _Tail], _Map, Table) ->
    Table;
iterate([{Node, N, Gateway}| Tail], Map, Table) ->
    case map:reachable(Node, Map) of
        [] ->
            iterate(Tail, Map, [{Node, Gateway} | Table]);
        Nodes ->
            UpdatedSorted = lists:foldl(fun(X, Acc) -> update(X, N+1, Gateway, Acc) end, Tail, Nodes),
            iterate(UpdatedSorted, Map, [{Node, Gateway} | Table])
    end.
