-module(hist).
-export([new/1, update/3]).

%returns a new history, where messages from Name will always be seen as old
new(Name) ->
    [{Name, 0}].

%checks if message number N from the Node is old or new
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {Node, M} ->
            case N > M of
                true ->
                    {new, lists:keyreplace(Node, 1, History, {Node, N})};
                false ->
                    old
            end;
        false ->
            {new, [{Node, N}|History]}
    end.
