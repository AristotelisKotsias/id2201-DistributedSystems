-module(rudy).

-export([start/1, stop/0, init/1, handler/1, request/1, reply/1]).

%Initiates a process with the name "rudy" and associates it with its Pid
start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)). 

%Terminates the process named "rudy"
stop() ->
    exit(whereis(rudy), "time to die").

%init(Port):the procedure that will initialize the server, takes a port
%number (for example 8080), opens a listening socket and passes the
%socket to handler/1. Once the request has been handled the socket
%will be closed.
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} -> 
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
    end.

%handler(Listen): will listen to the socket for an incoming connection.
%Once a client has connect it will pass the connection to request/1.
%When the request is handled the connection is closed.
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            error
    end.

%request(Client): will read the request from the client connection
%and parse it. It will then parse the request using your http parser and
%pass the request to reply/1. The reply is then sent back to the client.
request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
gen_tcp:close(Client).

%reply(Request): this is where we decide what to reply, how to turn
%the reply into a well formed HTTP reply.
reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).