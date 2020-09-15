-module(map).
-compile(export_all).

%returns an empty map (a empty list)
new() ->
    [].

%updates the Map to reflect that Node has directional links to all nodes in the list 
%Links. The old entry is removed. 
%keyfind(Key, N, TupleList) -> Tuple | false, 
%keydelete(Key, N, TupleList1) -> TupleList2
update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {_, _} ->
            [{Node,Links} | lists:keydelete(Node, 1, Map)];
        false ->
            [{Node, Links}|Map]
    end.

%returns the list of nodes directly reachable
%from Node
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {_, Links} ->
            Links;
        false ->
            []
    end.

%returns a list of all nodes in the map, also the ones without outgoing links
all_nodes(Map) ->
    lists:usort(lists:foldl(fun({Node, Links}, Nodes) -> [Node] ++ Links ++ Nodes end, [], Map)).

