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
			Upd = spawn(?MODULE, sendUpdate, [Socket]),
            spawn(?MODULE, psocket, [Socket, Upd, null]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    end,
    dispatcher(ListenSocket).

% envia mensajes de actualizacion al cliente.
sendUpdate(Socket) ->
	receive
		{playerJoined, [GameId, Player]} ->
			gen_tcp:send(Socket, "UPD 1 " ++ integer_to_list(GameId) ++ " "
			++ Player ++ " " ++ "se unio al juego \n"),
			receive
				{rp, 1, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		{continue, [GameId, P1, P2, Turn, Board]} ->
			gen_tcp:send(Socket, "UPD 2 " ++ integer_to_list(GameId)
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n"),
			receive
				{rp, 2, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		{win, [GameId, P1, P2, Turn, Board, Winner]} ->
			gen_tcp:send(Socket, "UPD 3 " ++ integer_to_list(GameId) ++ " "
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n"
			++ "gana " ++ Winner ++ "\n"),
			receive
				{rp, 3, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		{tie, [GameId, P1, P2, Turn, Board]} ->
			gen_tcp:send(Socket, "UPD 4 " ++ integer_to_list(GameId) ++ " "
			++ " turno:" ++ integer_to_list(Turn)  ++ " " ++ P1 ++ " vs " ++ P2 ++ "\n"
			++ prettyPrint(Board) ++ "\n"
			++ " empate" ++ "\n"),
			receive
				{rp, 4, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		{noGame, [GameId]} ->
			gen_tcp:send(Socket, "UPD 5 " ++ integer_to_list(GameId) ++ " juego cerrado \n"),
			receive
				{rp, 5, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		{abandon, [GameId, Player]} ->
			gen_tcp:send(Socket, "UPD 6 " ++ integer_to_list(GameId) ++ " " ++ Player ++ " abandona \n"),
			receive
				{rp, 6, Pcom} -> Pcom ! ok;
				{rp, _, Pcom} -> Pcom ! error
			end;
		die -> exit(kill);
		_ -> ok
	end,
	sendUpdate(Socket).


psocket(Socket, Upd, User) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node, _} ->
					ParsedCmd = string:tokens(string:strip(string:strip(binary_to_list(Cmd), right, $\n),right,$\r), " "),
                    spawn(Node, ?MODULE, pcomando, [ParsedCmd, self(), node(), User, Upd]),
                    io:format("pcomando se crea en ~p ~n",[Node])
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket]),
			exit(kill)
    end,
    receive
        {con, ok, CmdId, UserName} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, Upd, UserName);
        {con, error, CmdId, UserName} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, Upd, User);
        {lsg, ok, CmdId, GameList} -> A = io_lib:format("~p ~n",[GameList]), lists:flatten(A),
                                      gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ "Juegos \n" ++ A),
                                      psocket(Socket, Upd, User);
        {new, ok, CmdId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ "\n"), psocket(Socket, Upd, User);
        {acc, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {acc, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {pla, ok, CmdId, GameId, R, C} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ " " ++ R ++ " " ++ C ++ "\n"), psocket(Socket, Upd, User);
        {pla, error, CmdId, GameId, R, C} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ " " ++ R ++ " " ++ C ++ "\n"), psocket(Socket, Upd, User);
        {pla, ok, CmdId, GameId, leave} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ " " ++ "abandonar \n"), psocket(Socket, Upd, User);
				{pla, error, CmdId, GameId, leave} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ " " ++ "abandonar \n"), psocket(Socket, Upd, User);
				{obs, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {obs, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {leave, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {leave, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, Upd, User);
        {rp, ok} -> psocket(Socket, Upd, User);
        {rp, error, CmdId} -> gen_tcp:send(Socket, "ERROR UPD " ++ CmdId ++ "\n"), psocket(Socket, Upd, User);
        invalid -> gen_tcp:send(Socket, "Comando invalido \n"), psocket(Socket, Upd, User);
		    die -> gen_tcp:close(Socket), exit(kill)
    end.


pcomando(Cmd, Psocket, Node, User, Upd) ->
    case Cmd of
         ["CON", CmdId, UserName] when User == null ->
        {idusersManager, Node} ! {UserName, Upd, self()},
        receive
				 ok -> Psocket ! {con, ok, CmdId, UserName};
				 error -> Psocket ! {con, error, CmdId, UserName}
			 end;
	     ["CON", CmdId, UserName] when User =/= null ->
			 Psocket ! {con, error, CmdId, UserName};
         ["LSG", CmdId] when User =/= null ->
            idgamesManager ! {listgames, self()},
            receive
                GameList -> Psocket ! {lsg, ok, CmdId, GameList}
            end;
         ["NEW", CmdId] when User =/= null ->
            idgamesManager ! {newgame, self(), User, Upd},
            receive
                ok -> Psocket ! {new, ok, CmdId}
            end;
         ["ACC", CmdId, GameId] when User =/= null ->
            idgamesManager ! {accept, self(), User, Upd, list_to_integer(GameId)},
            receive
                ok -> Psocket ! {acc, ok, CmdId, GameId};
                error -> Psocket ! {acc, error, CmdId, GameId}
            end;
         ["PLA", CmdId, GameId, R, C] when User =/= null ->
            idgamesManager ! {play, self(), list_to_integer(GameId), User, list_to_integer(R), list_to_integer(C)},
            receive
                ok -> Psocket ! {pla, ok, CmdId, GameId, R, C};
                error -> Psocket ! {pla, error, CmdId, GameId, R, C}
            end;
         ["PLA", CmdId, GameId, "abandonar"] when User =/= null ->
            idgamesManager ! {abandon, self(), list_to_integer(GameId), User},
            receive
                ok -> Psocket ! {pla, ok, CmdId, GameId, leave};
				error -> Psocket ! {pla, error, CmdId, GameId, leave}
            end;
         ["OBS", CmdId, GameId] when User =/= null ->
		 	idgamesManager ! {obs, self(), list_to_integer(GameId), Upd, User},
			receive
				ok -> Psocket ! {obs, ok, CmdId, GameId};
				error -> Psocket ! {obs, error, CmdId, GameId}
			end;
         ["LEA", CmdId, GameId] when User =/= null ->
			 idgamesManager ! {leave, self(), list_to_integer(GameId), User, Upd},
		 	 receive
				 ok -> Psocket ! {leave, ok, CmdId, GameId};
				 error -> Psocket ! {leave, error, CmdId, GameId}
			 end;
         ["BYE"] when User =/= null ->
			 {idgamesManager, Node} ! {bye, User, Upd},
             {idusersManager, Node} ! {delete, User, Upd},
             Upd ! die,
			 Psocket ! die;
		 ["OK", CmdId] ->
		 	Upd ! {rp, list_to_integer(CmdId), self()},
			receive
				ok -> Psocket ! {rp, ok};
				error -> Psocket ! {rp, error, CmdId}
			end;
         _ -> Psocket ! invalid
    end.


% gestiona los juegos.
gamesManager(GameList, GamesCounter) ->
    receive
		{delete, GameId, PidGame} ->
			gamesManager(lists:delete({GameId, PidGame}, GameList), GamesCounter);
        {localGames, GlobGames} ->
            GlobGames ! GameList, gamesManager(GameList, GamesCounter);
        {localCount, GlobCount} ->
            GlobCount ! GamesCounter, gamesManager(GameList, GamesCounter);
        {newgame, Pcom, User, Upd} ->
            PidGame = spawn(?MODULE, game, [globalCounter(GamesCounter), {User, Upd}, {null, null}, [], ?CleanBoard]),
            Pcom ! ok,
            gamesManager([{GamesCounter, PidGame} | GameList], GamesCounter + 1);
        {listgames, Pcom} ->
			Pcom ! globalGames(GameList), gamesManager(GameList, GamesCounter);
		{leave, Pcom, GameId, User, Upd} ->
			case findGame(globalGames(GameList), GameId) of
			    error -> Pcom ! error, gamesManager(GameList, GamesCounter);
			 	{GameId, PidGame, _} ->
					PidGame ! {leave, User, Upd},
					Pcom ! ok,
					gamesManager(GameList, GamesCounter)
			end;
		{bye, User, Upd} -> lists:foreach(fun({GameId, PidGame, _}) ->
			PidGame ! {bye, User, Upd, node()},
			receive
				ok -> ok;
				{ok, delete, Node} ->
				  {idgamesManager, Node} ! {delete, GameId, PidGame}
		  	end
		end, globalGames(GameList)),
		gamesManager(GameList, GamesCounter);
        {accept, Pcom, User, Upd, GameId} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> Pcom ! error, gamesManager(GameList, GamesCounter);
                 {GameId, PidGame, _} ->
				 	PidGame ! {join, node(), User, Upd},
					receive
						ok -> Pcom ! ok;
						error -> Pcom ! error
					end,
					gamesManager(GameList, GamesCounter)
            end;
		{obs, Pcom, GameId, Upd, User} ->
			case findGame(globalGames(GameList), GameId) of
            error -> Pcom ! error;
            {GameId, PidGame, _} ->
              PidGame ! {obs, User, Upd, node()},
              receive
                ok -> Pcom ! ok;
                error -> Pcom ! error
              end
		    end, gamesManager(GameList, GamesCounter);
    {play, Pcom, GameId, Player, R, C} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> Pcom ! error, gamesManager(GameList, GamesCounter);
                 {GameId, PidGame, _} ->
                     PidGame ! {play, node(), Player, R, C},
                     receive
						 error -> Pcom ! error;
						 ok -> Pcom ! ok
					 end, gamesManager(GameList, GamesCounter)
            end;
		{abandon, Pcom, GameId, User} ->
	    	case findGame(globalGames(GameList), GameId) of
				  error -> Pcom ! error, gamesManager(GameList, GamesCounter);
			 	  {GameId, PidGame, _} ->
					  PidGame ! {abandon, node(), User},
					  receive
						  ok -> Pcom ! ok;
					  	  {ok, delete, Node} -> Pcom ! ok,
						  	{idgamesManager, Node} ! {delete, GameId, PidGame};
						  error -> Pcom ! error
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
		{join, Node, Player, Upd} when P2 == null andalso Player =/= P1 ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP1 | element(2, lists:unzip(Observers))], {playerJoined , [GameId, Player]}),
			game(GameId, {P1, UpdP1}, {Player, Upd}, lists:delete({Player, Upd}, Observers), {Turn, Board});
    {join, Node, Player, Upd} when P1 == null andalso Player =/= P2 ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP2 | element(2, lists:unzip(Observers))], {playerJoined , [GameId, Player]}),
			game(GameId, {Player, Upd}, {P2, UpdP2}, lists:delete({Player, Upd}, Observers), {Turn, Board});
		{join, Node, _, _}  ->
			{idgamesManager, Node} ! error,
			game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
    {obs, User, _, Node} when User == P1 orelse User == P2 ->
      {idgamesManager, Node} ! error,
			game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board});
    {obs, User, Upd, Node} ->
      case [Name || {Name, _} <- Observers, Name == User] of
        [] -> 
          {idgamesManager, Node} ! ok,
          game(GameId, {P1, UpdP1}, {P2, UpdP2}, [{User, Upd} | Observers], {Turn, Board});
        _ -> 
          {idgamesManager, Node} ! error,
          game(GameId, {P1, UpdP1}, {P2, UpdP2}, Observers, {Turn, Board})
        end;
		{leave, User, Upd} ->
			game(GameId, {P1, UpdP1},  {P2, UpdP2}, lists:delete({User, Upd}, Observers), {Turn, Board});
		{abandon, Node, Player} when Player == P1 andalso P2 =/= null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP2 | element(2, lists:unzip(Observers))], {abandon , [GameId, Player]}),
			game(GameId, {null, null}, {P2, UpdP2}, Observers, {Turn, Board});
		{abandon, Node, Player} when Player == P2 andalso P1 =/= null ->
			{idgamesManager, Node} ! ok,
			sendToUsers([UpdP1 | element(2, lists:unzip(Observers))], {abandon , [GameId, Player]}),
			game(GameId, {P1, UpdP2}, {null, null}, Observers, {Turn, Board});
		{abandon, Node, Player} when Player == P1 orelse Player == P2 ->
			{idgamesManager, Node} ! {ok, delete, node()},
			sendToUsers(element(2, lists:unzip(Observers)), {noGame , [GameId]}),
			exit(kill);
		{abandon, Node, _} -> {idgamesManager, Node} ! error;
		{bye, User, Upd, Node} when User == P1 andalso P2 =/= null ->
			NewObservers = lists:delete({User, Upd}, Observers),
			sendToUsers([UpdP2 | element(2, lists:unzip(NewObservers))], {abandon , [GameId, User]}),
			{idgamesManager, Node} ! ok,
			game(GameId, {null, null}, {P2, UpdP2}, NewObservers, ?CleanBoard);
		{bye, User, Upd, Node} when User == P2 andalso P1 =/= null ->
			NewObservers = lists:delete({User, Upd}, Observers),
			sendToUsers([UpdP1 | element(2, lists:unzip(NewObservers))], {abandon , [GameId, User]}),
			{idgamesManager, Node} ! ok,
			game(GameId, {P1, UpdP1}, {null, null}, NewObservers, ?CleanBoard);
		{bye, User, Upd, Node} when User == P1 orelse User == P2 ->
			{idgamesManager, Node} ! {ok, delete, node()},
			NewObservers = lists:delete({User, Upd}, Observers),
			sendToUsers(element(2, lists:unzip(NewObservers)), {noGame , [GameId]}),
			exit(kill);
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
							case gameOver(NewBoard, hd(Sym)) of
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
		{delete, User, Upd} ->
			usersManager(lists:delete({User, Upd}, UsersList));
        {UserName, Upd, Pcom} ->
            case [Name || {Name, _} <- globalUsers(UsersList), Name == UserName] of
				 [] -> Pcom ! ok, usersManager([{UserName, Upd} | UsersList]);
                 _ -> Pcom ! error, usersManager(UsersList)
            end
    end.


pbalance(LoadList) ->
    receive
        {Pid, where} -> Pid ! lists:nth(1, lists:keysort(2, LoadList)), pbalance(LoadList);
        {Node, Load} -> pbalance([{X,case X of Node -> Load; _ -> Y end} || {X,Y} <- LoadList])
    end.


pstat() ->
    [{idpbalance, Node} ! {node(), load()} || Node <- [node() | nodes()]],
    timer:sleep(1000),
    pstat().


% !!!!!!!!!!!!!!!!! ELIMINARLOS.
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
