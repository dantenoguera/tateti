-module(tp).
-compile(export_all).
-define (Board, [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]]).

%calcula la carga del nodo.
load() -> length(erlang:ports()).

%cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.
dispatcher(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nuevo cliente: ~p ~n", [Socket]),
            spawn(?MODULE, psocket, [Socket, unnamed]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    end,
    dispatcher(ListenSocket).

%por cada pedido, psocket crea un nuevo proceso (pcomando) que realiza todo cálculo necesario y
%le devuelva una respuesta a psocket, que le enviará al cliente. Además pcomando
%se encargará de generar los mensajes correspondientes para el resto de los clientes
%y mandarlos a sus respectivos psocket, de ser necesario.

psocket(Socket, User) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node, _} -> spawn(Node, ?MODULE, pcomando, [binary_to_list(Cmd), self(), node(), User]), % Crea el pcomando en el servidor correspondiente
                             io:format("pcomando se crea en ~p ~n",[Node])
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end,
    receive %rtas pcomando
        {con, ok, UserName} -> gen_tcp:send(Socket, "OK 0 " ++ UserName ++ "\n"), psocket(Socket, UserName);
        {con, error, UserName} -> gen_tcp:send(Socket, "ERROR 0 " ++ UserName ++ "\n"), psocket(Socket, unnamed);
        {new, ok} -> gen_tcp:send(Socket, "OK 2 \n"), psocket(Socket, User), psocket(Socket, User);
        {lsg, ok, GameList} -> A = io_lib:format("~p ~n",[GameList]), lists:flatten(A),
                               gen_tcp:send(Socket, ["Juegos \n" | A ]),
                                psocket(Socket, User);
        {acc, ok} -> gen_tcp:send(Socket, ["OK 3 \n"]);
        {acc, error} -> gen_tcp:send(Socket, ["ERROR 3 \n"])
    end.

pcomando(Cmd, PidPsocket, Node, User) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", UserName] -> idusersManager ! {UserName, self()},
                              receive
                                  ok -> PidPsocket ! {con, ok, UserName};
                                  error -> PidPsocket ! {con, error, UserName}
                              end;
         ["LSG"] -> idgamesManager ! {listgames, self()},
                    receive
                        GameList -> PidPsocket ! {lsg, ok, GameList}
                    end;
         ["NEW"] -> idgamesManager ! {newgame, self(), User},
                    receive
                        ok -> PidPsocket ! {new, ok}
                    end;
         ["ACC", GameId] -> idgamesManager ! {accept, self(), User, list_to_integer(GameId)},
                             receive
                                 error -> PidPsocket ! {acc, error};
                                 ok -> PidPsocket ! {acc, ok}
                             end;
         ["PLA", GameId, X, Y] -> idgamesManager ! {play, GameId, X, Y};
         ["PLA", _] -> ok;
         ["LEA"] -> ok;
         ["BYE"] -> ok;
         ["UPD"] -> ok
    end.

%uno por nodo
gamesManager(GameList, GamesCounter) ->
    receive
        {updateGame, GameId} -> {Node,PidGame, GameId, State} = findGame(GameList, GameId),
                                gamesManager([{Node, PidGame, GameId, "en curso"} | lists:delete({Node,PidGame, GameId, "en espera de contrincante"}, GameList)], GamesCounter);
        {localGames, PidglobGames} -> PidglobGames ! GameList, gamesManager(GameList, GamesCounter);
        {localCount, PidglobCount} -> PidglobCount ! GamesCounter, gamesManager(GameList, GamesCounter);
        {newgame, PidPcom, User} -> idpbalance ! {self(), where},
                                    receive
                                        {Node, _} -> PidGame = spawn(Node, ?MODULE, game, [User, GamesCounter, ?Board]),
                                                     PidPcom ! ok,
                                                     gamesManager([{Node,PidGame, GamesCounter, "en espera de contrincante"} | GameList], GamesCounter + 1)
                                    end;
        {listgames, PidPcom} -> PidPcom ! globalGames(GameList),
                                gamesManager(GameList, GamesCounter);
        {accept, PidPcom, User, GameId} -> io:format("hla! ~n"),
                                           case findGame(globalGames(GameList), GameId) of
                                                error -> ok;
                                                {Node,PidGame, GameId, State} -> PidGame ! {join, User},
                                                                                 case node() == Node of
                                                                                      true -> ok;
                                                                                      false -> {idgamesManager, Node} ! {updateGame, GameId}
                                                                                 end,
                                                                                 PidPcom ! ok,
                                                                                 gamesManager([{Node, PidGame, GameId, "en curso"} | lists:delete({Node,PidGame, GameId, State}, GameList)], GamesCounter)
                                           end;
        {play, GameId, X, Y} -> ok %revisar !!!!!!
    end.


findGame([], _) -> error;
findGame([{Node,PidGame, GameId, State} | T], Id) ->
    case GameId == Id of
         false -> findGame(T, Id);
         true -> {Node,PidGame, GameId, State}
    end.


game(J1, GameId, Board) ->
	io:format("J1: ~s ,GameId: ~p ~n", [J1, GameId]),
    receive
        {join, J2} -> io:format("J2: ~s", [J2])
    end.



% retorna un lista con los usuarios de todos los servers.
globalUsers(LocalUsers) ->
    LocalUsers ++ lists:append(lists:map( fun(Node)->
                {idgamesManager, Node} ! {localGames, self()},
                receive  Users -> Users end end,
            nodes())).


globalGames(LocalGames) ->
    LocalGames ++ lists:append(lists:map( fun(Node)->
                {idgamesManager, Node} ! {localGames, self()},
                receive  Games -> Games end end,
            nodes())).


globalCounter(LocalCount) ->
    Counters = lists:map( fun(Node)-> {idgamesManager, Node} ! {localCount, self()},
                                       receive  Count -> Count end end, nodes()),
    LocalCount + lists:foldl(fun(X, Sum) -> X + Sum end, 0, Counters).


%Maneja el tema de los usuarios
usersManager(UsersList) ->
    receive
        {localUsers, PidglobUsrs} -> PidglobUsrs ! UsersList, usersManager(UsersList);
        {UserName, PidPcom} -> case lists:member(UserName, globalUsers(UsersList)) of
                                    true -> PidPcom ! error, usersManager(UsersList);
                                    false -> PidPcom ! ok, usersManager([UserName | UsersList])
                               end
    end.


%recibe informacion de pstat, indica a psocket en que nodo crear pcomando.
%indica a gamesManager en que nodo crear el juego.
pbalance(LoadList) ->
    receive
        {Pid, where} -> Pid ! lists:nth(1, lists:keysort(2, LoadList)), pbalance(LoadList);
        {Node, Load} -> pbalance([{X,case X of Node -> Load; _ -> Y end} || {X,Y} <- LoadList])
    end.

%envia la carga de los nodos a pbalance
pstat() ->
    [{idpbalance, Node} ! {node(), load()} || Node <- [node() | nodes()]],
    timer:sleep(1000),
    pstat().

% crea el ListenSocket e inicializa pstat, pbalance

%-get_tcp:listen(0, [{active, false}]), crea el LisenSocket 0 indica que el sistema se encargará
% de buscar un puerto disponible, [{active, false}] es una opcion que ....

%se verifica que gen_tcp:listen no tire error.

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
