-module(test0).
-export([start/2]).

start(Module, N) ->
	io:format("-----------------------------------------------------------------------~n"),
	% checks probe
	Pid = test:start(Module),
	Name = 'node0',
	register(Name, Pid),
	io:format("-----------------------------------------------------------------------~n"),
	test:start(Module, N-1, Pid),
	timer:sleep(1000),
	Pid ! probe,
	timer:sleep(10),

	% used for adding a new node to an existing ring
	io:format("-----------------------------------------------------------------------~n"),
	Newkey = key:generate(),
	io:format("Nkey: ~w ~n", [Newkey]),
	Pid1 = node2:start(Newkey, Pid),
	% notify the ring of a new node
	Pid ! {notify, {Newkey, Pid1}},
	timer:sleep(1000),
	% check that the new node has been added by sending a probe msg
	Pid ! probe,
	timer:sleep(10),

	%checks store
	io:format("-----------------------------------------------------------------------~n"),
	Keys = test:keys(10),
	test:add(Keys, Pid),
	timer:sleep(1000),
	test:check(Keys, Pid),
	io:format("-----------------------------------------------------------------------~n"),
	Pid ! info,
	Pid1 ! info,
	timer:sleep(10),

	%checks that the store is splitted
	io:format("-----------------------------------------------------------------------~n"),
	Newkey1 = key:generate(),
	io:format("Nkey: ~w ~n", [Newkey1]),
	Pid2 = node2:start(Newkey1, Pid),
	% notify the ring of a new node
	Pid ! {notify, {Newkey1, Pid2}},
	timer:sleep(1000),
	Pid ! info,
	Pid1 ! info,
	Pid2 ! info.



