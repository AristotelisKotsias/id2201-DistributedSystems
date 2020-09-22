-module(test).
-export([start/0]).
%erl -name sweden@localhost -setcookie routy -connect_all false
start() ->
    routy:start(stockholm, stockholm),
    routy:start(malmo, malmo),
    routy:start(uppsala, uppsala),
    routy:start(kiruna, kiruna),
    routy:start(gothemborg, gothemborg),

    stockholm ! {add, uppsala, {uppsala, 'sweden@localhost'}},
    stockholm ! {add, malmo, {malmo, 'sweden@localhost'}},
    uppsala ! {add, kiruna, {kiruna, 'sweden@localhost'}},
    malmo ! {add, gothemborg, {gothemborg, 'sweden@localhost'}},
    gothemborg ! {add, kiruna, {kiruna, 'sweden@localhost'}},
    kiruna ! {add, stockholm, {stockholm, 'sweden@localhost'}},
    timer:sleep(1000),

    stockholm ! broadcast, 
    malmo ! broadcast,
    uppsala ! broadcast,
    kiruna ! broadcast,
    gothemborg ! broadcast,
    timer:sleep(1000),

    stockholm ! update,
    malmo ! update,
    uppsala ! update,
    kiruna ! update,
    gothemborg ! update,
    timer:sleep(1000),

    stockholm ! {send, stockholm, 'Hej'},
    timer:sleep(1000),

    stockholm ! {send, kiruna, 'Hej'},
    timer:sleep(1000),

    routy:stop(uppsala),
    timer:sleep(1000),
    stockholm ! update,
    stockholm ! {send, kiruna, 'Hej'},
    timer:sleep(1000),

    routy:stop(gothemborg),
    timer:sleep(1000),
    malmo ! update,
    stockholm ! {send, kiruna, 'Hej'}.
