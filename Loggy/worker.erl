-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, InternalTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, RcvTime, Msg} ->
            LamportTime = time:inc(Name, time:merge(InternalTime, RcvTime)),
            Log ! {log, Name, LamportTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, LamportTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        LamportTime = time:inc(Name, InternalTime),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, LamportTime, Message},
        jitter(Jitter),
        Log ! {log, Name, LamportTime, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, LamportTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).