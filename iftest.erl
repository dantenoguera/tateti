-module(iftest).
-export([init/0]).

init()->
    L = [1,2,3],
	if
		lists:member(1, L) -> "mal";
		true -> "bien"
    end.
    
    
	  
