-module(iftest).
-export([init/0]).

init()->
    L = [1,2,3],
	case lists:member(1, L) of
         true -> "bien";
         else -> "mal"
    end.