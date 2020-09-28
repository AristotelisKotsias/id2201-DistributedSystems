-module(logger1).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun()->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedQueue = [{From, Time, Msg} | Queue],
            SortedQueue = lists:keysort(2, UpdatedQueue),
            NotSafeQueue = safeToLog(SortedQueue, UpdatedClock),
            loop(UpdatedClock, NotSafeQueue);
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p ~n", [Time, From, Msg]).

safeToLog([{From, Time, Msg}|Tail], Clock) ->
    case time:safe(Time, Clock) of
        true ->
            log(From, Time, Msg),
            safeToLog(Tail, Clock);
        false ->
            [{From, Time, Msg}|Tail]
    end.