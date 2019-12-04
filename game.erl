-module(game).
-compile(export_all).
-define (Board, [[" ", " ", " "], 
                 [" ", " ", " "], 
                 [" ", " ", " "]]).
                 %No va a haber un problema si esto se llama Board y lo que recibe abajo tambien?


prettyPrint([])-> io:format("~n");
prettyPrint([[C1,C2, C3] | Board]) ->
	io:format("~s | ~s | ~s ~n", [C1, C2, C3]), 
	prettyPrint(Board).


insertSymFil([Fil | T], S, X, 1) -> 
	[insertSymCol(Fil, S, X) | T];
insertSymFil([Fil | T], S, X, Y) ->
	[Fil | insertSymFil(T, S, X, Y - 1)].

insertSymCol([Col | T], S, 1) ->
	case Col of 
	" " -> [S | T];
	_   -> [Col | T]
	end;
insertSymCol([Col | T], S, X) ->
	[Col | insertSymCol(T, S, X - 1)].
	
	
	

game(Player, Board, PidManager, X,Y) ->
	NewBoard = insertSym(Board, "X", X, Y),
	case (Board == NewBoard) of
	true -> case (Player == J1) of
	             true  -> PidManager ! (j1,Board); %Supuestamente hay un problema con la "," no se porque
	             false -> PidManager ! (j2,Board)
	        end;
	false -> case (Player == J1) of
	              true  -> PidManager ! (j2,NewBoard),
	              false -> PidManager ! (j1,NewBoard)
	         end
	end.
	
playerManager(Player, Board) ->
	prettyPrint (Board),
	io:format(Player ++ " inserte su jugada ~n"),
	(X,Y) = (1,2), %Aca se guarda la jugada insertada no sabia como hacerlo
	(NewPlayer, NewBoard) = game(Player, Board,self(),X,Y),
	playerManager(NewPlayer,NewBoard).
init() -> 
    spawn(?MODULE, playerManager, [j1,Board]).
	

