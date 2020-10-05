-module(test).

-compile(export_all).



% Used to create the first worker, try:
%
% W1 = test:first(1, gms1, 1000).
% W2 = test:first(1, gms2, 1000).
% W3 = test:first(1, gms3, 1000).


first(N, Module, Sleep) ->
   worker:start(N, Module, random:uniform(256), Sleep).

% Used to create additional workers, try:
%
%  test:add(4, gms1, W1, 1000). and 
%  test:add(4, gms2, W1, 1000). and ...
%  test:add(4, gms3, W3, 1000).

add(N, Module, Wrk, Sleep) ->
   worker:start(N, Module, random:uniform(256), Wrk, Sleep).

%% To create a number of workers in one go, 
% test:more(6, gms1, 1000).
% test:more(6, gms2, 1000).
% test:more(6, gms3, 1000).

more(N, Module, Sleep) when N > 1 ->
    Wrk = first(1, Module, Sleep),
    Ns = lists:seq(2,N),
    lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
    Wrk.
		      

% These are messages that we can send to one of the workers. It will
% multicast it to all workers. They should (if everything works)
% receive the message at the same (logical) time.

% test:freeze(W1).
freeze(Wrk) ->
    Wrk ! {send, freeze}.

% test:go(W1).
go(Wrk) ->
    Wrk ! {send, go}.

% test:sleep(W1,1000).
sleep(Wrk, Sleep) ->
    Wrk ! {send, {sleep, Sleep}}.

% test:stop(W1).
stop(Wrk) ->
    Wrk ! {send, stop}.


			  

















