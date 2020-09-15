-module(dijkstra).
-export([table/2, route/2]).

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

%constructs a routing table given the gateways and a map
table(Gateways, Map) ->
    %Nodes list includes the gateways too
    Nodes = map:all_nodes(Map),

    %Rest is a list of nodes without the gateways
    Rest = lists:filter(fun(Node) -> not lists:member(Node,Gateways) end, Nodes),

    Gtws = lists:map(fun (Node) -> {Node, 0, Node} end, Gateways),
    NotGtws = lists:map(fun (Node) -> {Node, inf, unknown} end, Rest),

    Sorted = Gtws ++ NotGtws,
    iterate(Sorted, Map, []).

%searches the routing table and return the gateway suitable to route messages 
%to a node
route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} ->
            {ok, Gateway};
        false ->
            notfound
    end.

    
