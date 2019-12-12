-module(tp).
-compile(export_all).
-define (CleanBoard, {0, "         "}).

% calcula la carga del nodo.
load() -> length(erlang:ports()).


% formatea el tablero para imprimir.
prettyPrint(Board) ->
	lists:sublist(Board, 1, 1) ++ " | "
	++ lists:sublist(Board, 2, 1) ++ " | "
	++ lists:sublist(Board, 3, 1) ++ "\n"
	++ lists:sublist(Board, 4, 1) ++ " | "
	++ lists:sublist(Board, 5, 1) ++ " | "
	++ lists:sublist(Board, 6, 1) ++ "\n"
	++ lists:sublist(Board, 7, 1) ++ " | "
	++ lists:sublist(Board, 8, 1) ++ " | "
	++ lists:sublist(Board, 9, 1).

% cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.
dispatcher(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nuevo cliente: ~p ~n", [Socket]),
			PidSendUpd = spawn(?MODULE, sendUpdate, [Socket]),
            spawn(?MODULE, psocket, [Socket, PidSendUpd, null]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    end,
    dispatcher(ListenSocket).


% envia mensajes de actualizacion al cliente.
sendUpdate(Socket) ->
	receive
		{playerJoined, [GameId, Player]} ->
			gen_tcp:send(Socket, "UPD 1 " ++ integer_to_list(GameId) ++ " "
			++ Player ++ " " ++ "se unio al juego \n");
		{continue, [GameId, P1, P2, Turn, Board]} ->
			gen_tcp:send(Socket, "UPD 2 " ++ integer_to_list(GameId)
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n");
		{win, [GameId, P1, P2, Turn, Board, Winner]} ->
			gen_tcp:send(Socket, "UPD 3 " ++ integer_to_list(GameId) ++ " "
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n"
			++ "gana " ++ Winner ++ "\n");
		{tie, [GameId, P1, P2, Turn, Board]} ->
			gen_tcp:send(Socket, "UPD 4 " ++ integer_to_list(GameId) ++ " "
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n"
			++ " empate" ++ "\n");
		{noGame, [GameId]} ->
			gen_tcp:send(Socket, "UPD 5 " ++ integer_to_list(GameId) ++ " juego cerrado \n");
		{left, [GameId, Player]} ->
			gen_tcp:send(Socket, "UPD 6 " ++ integer_to_list(GameId) ++ Player ++ " abandona \n")
	end,
	sendUpdate(Socket).

% por cada pedido, psocket crea un nuevo proceso (pcomando) que realiza todo cálculo necesario y
% le devuelva una respuesta a psocket, que le enviará al cliente. Además pcomando
% se encargará de generar los mensajes correspondientes para el resto de los clientes
% y mandarlos a sus respectivos psocket, de ser necesario.
psocket(Socket, PidSendUpd, User) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node, _} ->
                    spawn(Node, ?MODULE, pcomando, [binary_to_list(Cmd), self(), node(), User, PidSendUpd]),
                    io:format("pcomando se crea en ~p ~n",[Node])
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end,
    receive %rtas pcomando
        {con, ok, CmdId, UserName} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, PidSendUpd, UserName);
        {con, error, CmdId, UserName} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, PidSendUpd, null);
        {lsg, ok, CmdId, GameList} -> A = io_lib:format("~p ~n",[GameList]), lists:flatten(A),
                                      gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ "Juegos \n" ++ A),
                                      psocket(Socket, PidSendUpd, User);
        {new, ok, CmdId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ "\n"), psocket(Socket, PidSendUpd, User);
        {acc, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
        {acc, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
        {pla, ok, CmdId, GameId, R, C} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ " " ++ R ++ " " ++ C ++ "\n"), psocket(Socket, PidSendUpd, User);
        {pla, error, CmdId, GameId, R, C} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ " " ++ R ++ " " ++ C ++ "\n"), psocket(Socket, PidSendUpd, User);
        {pla, ok, CmdId, GameId, leave} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ " " ++ "abandonar \n"), psocket(Socket, PidSendUpd, User);
		{pla, error, CmdId, GameId, leave} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ " " ++ "abandonar \n"), psocket(Socket, PidSendUpd, User);
		{obs, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
		{obs, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
		{leave, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
		{leave, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERRor " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, PidSendUpd, User);
        invalid -> gen_tcp:send(Socket, "Comando invalido \n"), psocket(Socket, PidSendUpd, User)
    end.


pcomando(Cmd, PidPsocket, Node, User, PidSendUpd) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", CmdId, UserName] when User == null ->
			 {idusersManager, Node} ! {UserName, PidSendUpd, self()},
			 receive
				 ok -> PidPsocket ! {con, ok, CmdId, UserName};
				 error -> PidPsocket ! {con, error, CmdId, UserName}
			 end;
	     ["CON", CmdId, UserName] when User =/= null ->
			 PidPsocket ! {con, error, CmdId, UserName};
         ["LSG", CmdId] when User =/= null ->
            idgamesManager ! {listgames, self()},
            receive
                GameList -> PidPsocket ! {lsg, ok, CmdId, GameList}
            end;
         ["NEW", CmdId] when User =/= null ->
            idgamesManager ! {newgame, self(), User, PidSendUpd},
            receive
                ok -> PidPsocket ! {new, ok, CmdId}
            end;
         ["ACC", CmdId, GameId] when User =/= null ->
            idgamesManager ! {accept, self(), User, PidSendUpd, list_to_integer(GameId)},
            receive
                ok -> PidPsocket ! {acc, ok, CmdId, GameId};
                error -> PidPsocket ! {acc, error, CmdId, GameId}
            end;
         ["PLA", CmdId, GameId, R, C] when User =/= null ->
            idgamesManager ! {play, self(), list_to_integer(GameId), User, list_to_integer(R), list_to_integer(C)},
            receive
                ok -> PidPsocket ! {pla, ok, CmdId, GameId, R, C};
                error -> PidPsocket ! {pla, error, CmdId, GameId, R, C}
            end;
         ["PLA", CmdId, GameId, "abandonar"] when User =/= null ->
            idgamesManager ! {abandon, self(), list_to_integer(GameId), User},
            receive
                ok -> PidPsocket ! {pla, ok, CmdId, GameId, leave};
				error -> PidPsocket ! {pla, error, CmdId, GameId, leave}
            end;
         ["OBS", CmdId, GameId] when User =/= null ->
		 	idgamesManager ! {obs, self(), list_to_integer(GameId), PidSendUpd, User},
			receive
				ok -> PidPsocket ! {obs, ok, CmdId, GameId};
				error -> PidPsocket ! {obs, error, CmdId, GameId}
			end;
         ["LEA", CmdId, GameId] when User =/= null ->
			 idgamesManager ! {leave, self(), list_to_integer(GameId), User, PidSendUpd},
		 	 receive
				 ok -> PidPsocket ! {leave, ok, CmdId, GameId};
				 error -> PidPsocket ! {leave, error, CmdId, GameId}
			 end;
         ["BYE", CmdId] when User =/= null -> ok;
         _ -> PidPsocket ! invalid
    end.


% gestiona los juegos.
gamesManager(GameList, GamesCounter) ->
    receive
        {localGames, PidglobGames} ->
            PidglobGames ! GameList, gamesManager(GameList, GamesCounter);
        {localCount, PidglobCount} ->
            PidglobCount ! GamesCounter, gamesManager(GameList, GamesCounter);
        {newgame, PidPcom, User, Upd} ->
            PidGame = spawn(?MODULE, game, [globalCounter(GamesCounter), {User, Upd}, {null, null}, [], ?CleanBoard]),
            PidPcom ! ok,
            gamesManager([{GamesCounter, PidGame} | GameList], GamesCounter + 1);
        {listgames, PidPcom} ->
			PidPcom ! globalGames(GameList), gamesManager(GameList, GamesCounter);
		{leave, PidPcom, GameId, User, PidSendUpd} ->
			case findGame(globalGames(GameList), GameId) of
			    error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
			 	{GameId, PidGame, _} ->
					PidGame ! {leave, User, PidSendUpd},
					gamesManager(GameList, GamesCounter)
			end;
        {accept, PidPcom, User, Upd, GameId} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
                 {GameId, PidGame, _} ->
				 	PidGame ! {join, node(), User, Upd},
					receive
						ok -> PidPcom ! ok;
						error -> PidPcom ! error
					end,
					gamesManager(GameList, GamesCounter)
            end;
		{obs, PidPcom, GameId, PidSendUpd, User} ->
			case findGame(globalGames(GameList), GameId) of
                 error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
			 	  {GameId, PidGame, _} -> PidPcom ! ok,
				  	PidGame ! {obs, User, PidSendUpd},
					gamesManager(GameList, GamesCounter)
		    end;
        {play, PidPcom, GameId, Player, R, C} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
                 {GameId, PidGame, _} ->
                     PidGame ! {play, node(), Player, R, C},
                     receive
						 error -> PidPcom ! error;
						 ok -> PidPcom ! ok
					 end, gamesManager(GameList, GamesCounter)
            end;
		{abandon, PidPcom, GameId, User} ->
	    	case findGame(globalGames(GameList), GameId) of
				  error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
			 	  {GameId, PidGame, _} ->
					  PidGame ! {left, node(), User},
					  receive
						  ok -> PidPcom ! ok;
						  error -> PidPcom ! error
					  end,
					  gamesManager(GameList, GamesCounter)
			end
    end.

sendToUsers(Users, Data) ->
	lists:foreach(fun(Usr) -> Usr ! Data end, Users).

game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board}) ->
    receive
		{state, Pid} ->
			if
				P1 =/= null orelse P2 =/= null -> Pid ! "en espera de contrincante";
			    true -> Pid ! "en curso"
			end,
			game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
		{join, Node, Player, Upd} when P2 == null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP1 | element(2, lists:unzip(Observers))], {playerJoined , [GameId, Player]}),
			game(GameId, {P1, UpdP1}, {Player, Upd}, Observers, {Turn, Board});
        {join, Node, Player, Upd} when P1 == null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP2 | element(2, lists:unzip(Observers))], {playerJoined , [GameId, Player]}),
			game(GameId, {Player, Upd}, {P2, UpdP2}, Observers, {Turn, Board});
		{join, Node, _}  ->
			{idgamesManager, Node} ! error,
			game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
		{obs, User, PidSendUpd} ->
			game(GameId, P1, P2, [{User, PidSendUpd} | Observers], {Turn, Board});
		{leave, User, PidSendUpd} ->
			game(GameId, {P1, UpdP1},  {P2, UpdP2}, lists:delete({User, PidSendUpd}, Observers), {Turn, Board});
		{left, Node, Player} when Player == P1 andalso P2 =/= null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP2 | element(2, lists:unzip(Observers))], {left , [GameId, Player]}),
			game(GameId, {null, null}, {P2, UpdP2}, Observers, {Turn, Board});
		{left, Node, Player} when Player == P2 andalso P1 =/= null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP1 | element(2, lists:unzip(Observers))], {left , [GameId, Player]}),
			game(GameId, {P1, UpdP2}, {null, null}, Observers, {Turn, Board});
		{left, Node, Player} when Player == P1 orelse Player == P2 ->
			{idgamesManager, Node} ! ok,
			sendToUsers(element(2, lists:unzip(Observers)), {noGame , [GameId]}),
			exit(kill);
		{left, Node, _} -> {idgamesManager, Node} ! error;
        {play, Node, Player, R, C} when P1 =/= null andalso P2 =/= null ->
			case validateTurn(Player, P1, P2, Turn) of
				 error ->
				 	{idgamesManager, Node} ! error,
					game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
			 	 Sym ->
				 	case processPlay(Board, Sym, R, C) of
					     error ->
						 	{idgamesManager, Node} ! error,
							game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
					     NewBoard ->
							{idgamesManager, Node} ! ok,
							case gameOver(NewBoard, Sym) of
                            	continue ->
									sendToUsers([UpdP1, UpdP2 | element(2, lists:unzip(Observers))], {continue , [GameId, P1, P2, Turn, NewBoard]}),
 									game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn + 1, NewBoard});
								win ->
									sendToUsers([UpdP1, UpdP2 | element(2, lists:unzip(Observers))], {win, [GameId, P1, P2, Turn, NewBoard, Player]}),
									game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, ?CleanBoard);
                                tie ->
									sendToUsers([UpdP1, UpdP2 | element(2, lists:unzip(Observers))], {tie, [GameId, P1, P2, Turn, NewBoard]}),
									game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, ?CleanBoard)
                            end
                    end
			end;
		{play, Node, _, _, _} ->
			{idgamesManager, Node} ! error,
			game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board})
    end.


% determina si un jugador gano.
win(Board,S) ->
  case Board of
    [S,S,S,_,_,_,_,_,_] -> true;
    [_,_,_,S,S,S,_,_,_] -> true;
    [_,_,_,_,_,_,S,S,S] -> true;
    [S,_,_,_,S,_,_,_,S] -> true;
    [_,_,S,_,S,_,S,_,_] -> true;
    [_,_,S,_,_,S,_,_,S] -> true;
    [S,_,_,S,_,_,S,_,_] -> true;
    [_,S,_,_,S,_,_,S,_] -> true;
    _                   -> false
  end.

% devulve el estado de desarrollo del juego.
gameOver(Board, S) ->
	case win(Board, S) of
		 true -> win;
	 	 false -> case lists:member(32, Board) of
			           true -> continue;
		               false -> tie
				 end
	end.

% procesa una jugada.
processPlay(Board, Sym, R, C) ->
	if
		((R > 0) and (R < 4) and (C > 0) and (C < 4)) ->
			S = lists:nth(3*(R-1) + C,Board),
	        if S == 32 ->
				lists:sublist(Board, 3 * (R - 1) + C - 1)
				++ Sym ++ lists:nthtail(3*(R-1)+C,Board);
				true -> error
			end;
		true -> error
	end.

% valida el turno de un jugador
validateTurn(Player, P1, P2, Turn) ->
	if Player == P1 andalso (Turn rem 2) == 0 -> "O";
	   Player == P2 andalso (Turn rem 2) == 1 -> "X";
       true -> error
   end.


% busca a un juego en una lista de juegos.
findGame([], _) -> error;
findGame([{GameId, PidGame, State} | T], Id) ->
    case GameId == Id of
         false -> findGame(T, Id);
         true -> {GameId, PidGame, State}
    end.


% retorna un lista con los usuarios de todos los servers.
globalUsers(LocalUsers) ->
    LocalUsers ++ lists:append(lists:map( fun(Node)->
                {idusersManager, Node} ! {localUsers, self()},
                receive  Users -> Users end end,
            nodes())).



% retorna un lista con los juegos de todos los servers.
globalGames(LocalGames) ->
	lists:map(fun({GameId, PidGame}) ->
		PidGame ! {state, self()},
		receive
			State -> {GameId, PidGame, State}
		end end,LocalGames)
   	++
    lists:append(lists:map(fun(Node)->
    	{idgamesManager, Node} ! {localGames, self()},
    	receive
			Games ->
				lists:map(fun({GameId, PidGame}) ->
					PidGame ! {state, self()},
					receive
						State -> {GameId, PidGame, State}
					end end, Games)
		end end, nodes())).


% retorna la suma total de juegos.
globalCounter(LocalCount) ->
    Counters = lists:map( fun(Node)-> {idgamesManager, Node} ! {localCount, self()},
                                       receive  Count -> Count end end, nodes()),
    LocalCount + lists:foldl(fun(X, Sum) -> X + Sum end, 0, Counters).


% maneja el registro de usuarios.
usersManager(UsersList) ->
    receive
        {localUsers, Pid} -> Pid ! UsersList, usersManager(UsersList);
        {globalUsers, Pid} -> Pid ! globalUsers(UsersList), usersManager(UsersList);
        {UserName, PidSendUpd, PidPcom} ->
            case [Name || {Name, _, _} <- globalUsers(UsersList), Name == UserName] of
				 [] -> PidPcom ! ok, usersManager([{UserName, PidSendUpd} | UsersList]);
                 _ -> PidPcom ! error, usersManager(UsersList)
            end
    end.


% recibe informacion de pstat, indica a psocket en que nodo crear pcomando.
% indica a gamesManager en que nodo crear el juego.
pbalance(LoadList) ->
    receive
        {Pid, where} -> Pid ! lists:nth(1, lists:keysort(2, LoadList)), pbalance(LoadList);
        {Node, Load} -> pbalance([{X,case X of Node -> Load; _ -> Y end} || {X,Y} <- LoadList])
    end.

% envia la carga de los nodos a pbalance.
pstat() ->
    [{idpbalance, Node} ! {node(), load()} || Node <- [node() | nodes()]],
    timer:sleep(1000),
    pstat().

% crea el ListenSocket e inicializa pstat, pbalance.
inita(Port) ->
    connect('nB@127.0.0.1'),
    case gen_tcp:listen(Port, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(idusersManager, spawn(?MODULE, usersManager, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [0 || _<- [node() | nodes()]])])),
            register(idgamesManager, spawn(?MODULE, gamesManager, [[], 0]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.

initb(Port) ->
    connect('nA@127.0.0.1'),
    case gen_tcp:listen(Port, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(idusersManager, spawn(?MODULE, usersManager, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [0 || _<- [node() | nodes()]])])),
            register(idgamesManager, spawn(?MODULE, gamesManager, [[], 0]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.

init(Port) ->
    case gen_tcp:listen(Port, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(idusersManager, spawn(?MODULE, usersManager, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [0 || _<- [node() | nodes()]])])),
            register(idgamesManager, spawn(?MODULE, gamesManager, [[], 0]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.

connect(Node) ->
  case net_adm:ping(Node) of
    pang -> connect(Node);
    pong -> io:format("Conectado a ~p ~n", [Node])
  end.
