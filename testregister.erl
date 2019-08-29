-module(testregister).
-export([init/0, f1/0, f2/0]).

f1() ->
    receive
        {hola} -> io:format("Roger that~n")
    end.

f2() ->
    p1 ! {hola}.


init() ->
    register(p1, spawn(?MODULE, f1,[])),
    spawn(?MODULE, f2, []).
