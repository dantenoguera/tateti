-module(game).
-compile(export_all).
-define (Board, [[" ", " ", " "], 
                 [" ", " ", " "], 
                 [" ", " ", " "]]).


prettyPrint([])-> io:format("~n");
prettyPrint([[C1,C2, C3] | Board]) ->
	io:format("~s | ~s | ~s ~n", [C1, C2, C3]), 
	prettyPrint(Board).


insertSymFil([Fil | T], S, X, 1) -> ;
	[insertSymCol(Fil, S, X) | T].
insertSymFil([Fil | T], S, X, Y) ->
	[Fil | insertSymFil(T, S, X, Y - 1)].

insertSymCol([Col | T], S, 1) ->
	
	
	
	
	

game(Board) ->
	receive
		{X, Y} -> NewBoard = insertSym(Board, "X", X, Y)
	end.

init() -> 
    spawn(?MODULE, game, [Board]).
	

