-module(tp).
-compile(export_all).
-define (CleanBoard, [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]]).

% calcula la carga del nodo.
load() -> length(erlang:ports()).

% imprime el tablero
prettyPrint(Socket, GameId, P1, P2, Board) ->
	gen_tcp:send(Socket, "imprime todos los datos \n").


% cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.
dispatcher(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nuevo cliente: ~p ~n", [Socket]),
            spawn(?MODULE, psocket, [Socket, unnamed]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    end,
    dispatcher(ListenSocket).

% por cada pedido, psocket crea un nuevo proceso (pcomando) que realiza todo cálculo necesario y
% le devuelva una respuesta a psocket, que le enviará al cliente. Además pcomando
% se encargará de generar los mensajes correspondientes para el resto de los clientes
% y mandarlos a sus respectivos psocket, de ser necesario.
psocket(Socket, User) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node, _} ->
                    spawn(Node, ?MODULE, pcomando, [binary_to_list(Cmd), self(), node(), User]),
                    io:format("pcomando se crea en ~p ~n",[Node])
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end,
    receive %rtas pcomando
        {con, ok, CmdId, UserName} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, UserName);
        {con, error, CmdId, UserName} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ UserName ++ "\n"), psocket(Socket, unnamed);
        {lsg, ok, CmdId, GameList} -> A = io_lib:format("~p ~n",[GameList]), lists:flatten(A),
                                      gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ "Juegos \n" ++ A),
                                      psocket(Socket, User);
        {new, ok, CmdId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ "\n"), psocket(Socket, User);
        {acc, ok, CmdId, GameId} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, User);
        {acc, error, CmdId, GameId} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ "\n"), psocket(Socket, User);
        {pla, ok, CmdId, GameId, X, Y} -> gen_tcp:send(Socket, "OK " ++ CmdId ++ " " ++ GameId ++ " " ++ X ++ " " ++ Y ++ "\n"), psocket(Socket, User);
        {pla, error, CmdId, GameId, X, Y} -> gen_tcp:send(Socket, "ERROR " ++ CmdId ++ " " ++ GameId ++ " " ++ X ++ " " ++ Y ++ "\n"), psocket(Socket, User);
        invalid -> gen_tcp:send(Socket, "Comando Invalido \n"), psocket(Socket, User),
    end.

pcomando(Cmd, Socket, PidPsocket, Node, User) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", CmdId, UserName] ->
            case User == unnamed of
                 false -> PidPsocket ! {con, error, CmdId, UserName};
                 true ->
                     idusersManager ! {UserName, self()},
                     receive
                         ok -> PidPsocket ! {con, ok, CmdId, UserName};
                         error -> PidPsocket ! {con, error, CmdId, UserName}
                     end
            end;
         ["LSG", CmdId] ->
            idgamesManager ! {listgames, self()},
            receive
                GameList -> PidPsocket ! {lsg, ok, CmdId, GameList}
            end;
         ["NEW", CmdId] ->
            idgamesManager ! {newgame, self(), User, Socket},
            receive
                ok -> PidPsocket ! {new, ok, CmdId}
            end;
         ["ACC", CmdId, GameId] ->
            idgamesManager ! {accept, self(), User, PidPsocket, list_to_integer(GameId)},
            receive
                ok -> PidPsocket ! {acc, ok, CmdId, GameId};
                error -> PidPsocket ! {acc, error, CmdId, GameId}
            end;
         ["PLA", CmdId, GameId, X, Y] ->
            idgamesManager ! {play, self(), list_to_integer(GameId), User, X, Y},
            receive
                ok -> PidPsocket ! {pla, ok, CmdId, GameId, X, Y};
                error -> PidPsocket ! {pla, error, CmdId, GameId, X, Y}
            end;
         ["PLA", CmdId, _] -> ok;
         ["LEA", CmdId] -> ok;
         ["BYE", CmdId] -> ok;
         _ -> PidPsocket ! invalid
    end.


%{node(),PidGame, GamesCounter, {User, PidPsocket}, {null, null}, [], "en espera de contrincante"}

gamesManager(GameList, GamesCounter) ->
    receive
        {updateGame, GameId} ->
            {Node,PidGame, GameId, {P1, PS1}, {P2, PS2}, Obs, _} = findGame(GameList, GameId),
            gamesManager([{Node, PidGame, GameId, {P1, PS1}, {P2, PS2}, Obs, "en curso"} |
                lists:delete({Node,PidGame, GameId, {P1, PS1}, {P2, PS2}, Obs, "en espera de contrincante"}, GameList)],
            GamesCounter);
        {localGames, PidglobGames} ->
            PidglobGames ! GameList, gamesManager(GameList, GamesCounter);
        {localCount, PidglobCount} ->
            PidglobCount ! GamesCounter, gamesManager(GameList, GamesCounter);
        {newgame, PidPcom, User, PidPsocket} ->
            PidGame = spawn(?MODULE, game, [User, unnamed, ?CleanBoard]),
            PidPcom ! ok,
            gamesManager([{node(),PidGame, GamesCounter, {User, PidPsocket}, {null, null}, [], "en espera de contrincante"} | GameList], GamesCounter + 1);
        {listgames, PidPcom} -> PidPcom ! globalGames(GameList),  gamesManager(GameList, GamesCounter);
        {accept, PidPcom, User, PidPsocket, GameId} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
                 {Node,PidGame, GameId, {P1, PS1}, {null, null}, [], "en espera de contrincante"} ->
                    PidGame ! {join, User},
                    PidPcom ! ok,
                    case node() == Node of
                         true -> gamesManager([{Node, PidGame, GameId, {P1, PS1}, {User, PidPsocket}, [], "en curso"} | lists:delete({Node,PidGame, GameId, {P1, PS1}, {null, null}, [], "en espera de contrincante"}, GameList)], GamesCounter);
                         false -> {idgamesManager, Node} ! {updateGame, GameId}, gamesManager(GameList, GamesCounter)
                    end
            end;
        {play, PidPcom, GameId, Player, X, Y} ->
            case findGame(globalGames(GameList), GameId) of
                 error -> PidPcom ! error, gamesManager(GameList, GamesCounter);
                 {_,PidGame, GameId, {P1, PS1}, {P2, PS2}, _, _} ->
                     PidGame ! {play, node(), Player, list_to_integer(X), list_to_integer(Y)},
                     receive
                         {ok, NewBoard} ->
                             PidPcom ! ok,
                             PS1 ! {newboard, GameId, P1, P2, NewBoard},
                             PS2 ! {newboard, GameId, P1, P2, NewBoard};
                         error -> PidPcom ! error
                     end
            end, gamesManager(GameList, GamesCounter)
    end.


findGame([], _) -> error;
findGame([{Node,PidGame, GameId, {P1, PS1}, {P2, PS2}, Obs, State} | T], Id) ->
    case GameId == Id of
         false -> findGame(T, Id);
         true -> {Node,PidGame, GameId, {P1, PS1}, {P2, PS2}, Obs, State}
    end.


game(P1, P2, Board) ->
    receive
        {join, Player} -> game(P1, Player, Board);
        {play, Node, Player, X, Y} ->
            io:format("Jugada recibida ~n"),
            case P2 of % no se pueden realizar jugadas hasta que haya dos jugadores
                 unnamed -> {idgamesManager, Node} ! error;
                 _ -> case Player == P1 of
                           false ->
                               {idgamesManager, Node} ! error, game(P1, P2, Board);
                           true ->
                               case processPlay(Board, X ,Y) of
                                    error ->
                                        {idgamesManager, Node} ! error, game(P1, P2, Board);
                                    NewBoard ->
                                        {idgamesManager, Node} ! {ok, NewBoard},
                                        game(P2, P1, NewBoard)
                               end
                      end
            end
    end.

processPlay(_, _, _) -> ?CleanBoard.

% retorna un lista con los usuarios de todos los servers.
globalUsers(LocalUsers) ->
    LocalUsers ++ lists:append(lists:map( fun(Node)->
                {idusersManager, Node} ! {localUsers, self()},
                receive  Users -> Users end end,
            nodes())).

% retorna un lista con los juegos de todos los servers.
globalGames(LocalGames) ->
    LocalGames ++ lists:append(lists:map( fun(Node)->
                {idgamesManager, Node} ! {localGames, self()},
                receive  Games -> Games end end,
            nodes())).

% retorna la suma total de juegos.
globalCounter(LocalCount) ->
    Counters = lists:map( fun(Node)-> {idgamesManager, Node} ! {localCount, self()},
                                       receive  Count -> Count end end, nodes()),
    LocalCount + lists:foldl(fun(X, Sum) -> X + Sum end, 0, Counters).


% maneja el registro de usuarios.
usersManager(UsersList) ->
    receive
        {localUsers, Pid} -> Pid ! UsersList, usersManager(UsersList);
        {globalUsers, Pid} -> Pid ! globalUsers(UsersList);
        {UserName, PidPcom} ->
            case lists:member(UserName, globalUsers(UsersList)) of
                 true -> PidPcom ! error, usersManager(UsersList);
                 false -> PidPcom ! ok, usersManager([UserName | UsersList])
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
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [999 || _<- [node() | nodes()]])])),
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
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [999 || _<- [node() | nodes()]])])),
            register(idgamesManager, spawn(?MODULE, gamesManager, [[], 0]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.

init(Port) ->
    case gen_tcp:listen(Port, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(idusersManager, spawn(?MODULE, usersManager, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [999 || _<- [node() | nodes()]])])),
            register(idgamesManager, spawn(?MODULE, gamesManager, [[], 0]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.

connect(Node) ->
  case net_adm:ping(Node) of
    pang -> connect(Node);
    pong -> io:format("Conectado a ~p ~n", [Node])
  end.
