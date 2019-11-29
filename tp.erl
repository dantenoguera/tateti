-module(tp).
-compile(export_all).

%calcula la carga del nodo.
load() -> length(erlang:ports()).

%cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.

%gen_tcp:accept(ListenSocket) acepta un request de conexion que viene del LisenSocket devuelve
%{ok, Socket}
%{error, closed} si listen socket se cerro

%control de error en gen_tcp:accept(ListenSocket)

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

%%- gen_tcp:recv(Socket, 0). Recive un paquete de un socket, 0 indica que pude ser de longitud arbitraria,
%devuelve ok con el paquete o error

%-control de error en gen_tcp:recv(Socket, 0)
psocket(Socket, User) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node, _} -> spawn(Node, ?MODULE, pcomando, [binary_to_list(Cmd), self(), User]) % Crea el pcomando en el servidor correspondiente
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end,
    %rtas pcomando
    receive
        {con, ok, UserName} -> gen_tcp:send(Socket, "OK 0 " ++ UserName ++ "\n"), psocket(Socket, UserName);
        {con, error, UserName} -> gen_tcp:send(Socket, "ERROR 0 " ++ UserName ++ "\n"), psocket(Socket, unnamed);
        {new, ok} -> gen_tcp:send(Socket, "OK 2 \n"), psocket(Socket, User);
        {lsg, ok, GameList, WaitList} -> A = io_lib:format("~p ~n",[GameList]), lists:flatten(A),
                                         B = io_lib:format("~p ~n",[WaitList]), lists:flatten(B),
                                         gen_tcp:send(Socket, ["Juegos en curso \n" | A ]),
                                         gen_tcp:send(Socket, ["Juegos en espera de contrincante \n" | B]),
                                         psocket(Socket, User)

    end.

pcomando(Cmd, PidPsocket, User) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", UserName] -> iduserlisthandler ! {UserName, self()},
                              receive
                                  ok -> PidPsocket ! {con, ok, UserName};
                                  error -> PidPsocket ! {con, error, UserName}
                              end;
         ["LSG", _] -> idgamesHandler ! {self(), listgames},
                       receive
                           {_, GameList, WaitList} -> PidPsocket ! {lsg, ok, GameList, WaitList}
                       end;
         ["NEW", _] -> idgamesHandler ! {self(), newgame, User},
                       receive
                           ok -> PidPsocket ! {new, ok}
                       end;
         ["ACC"] -> ok;
         ["PLA"] -> ok;
         ["LEA"] -> ok;
         ["BYE"] -> ok;
         ["UPD"] -> ok
    end.


%uno por nodo?
gamesHandler(Node, NodeList, GameList, WaitList) ->
    receive
        {PidPcom, newgame, User} -> PidPcom ! ok,
                                    gamesHandler(Node, NodeList, GameList, [{User, length(GameList) + 1} | WaitList]);
        {PidPcom, listgames} -> PidPcom ! {ok, GameList, WaitList},
                                gamesHandler(Node, NodeList, GameList, WaitList)
    end.

game() -> ok.



%Maneja el tema de los usuarios
userlisthandler(UserNameList) ->
    receive
        {UserName, PidPcom} -> case lists:member(UserName, UserNameList) of
                                    true -> PidPcom ! error, userlisthandler(UserNameList);
                                    false -> PidPcom ! ok, userlisthandler([UserName | UserNameList])
                               end
    end.


%recibe informacion de pstat, indica a psocket en que nodo crear pcomando. Primero
% envia a pcomando despues actualiza la lista.
pbalance(LoadList) ->
    receive
        {PidPsocket, where} -> PidPsocket ! lists:nth(1, lists:keysort(2, LoadList)), pbalance(LoadList);
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

init(Port) ->
    case gen_tcp:listen(Port, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(iduserlisthandler, spawn(?MODULE, userlisthandler, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip([node() | nodes()], [999 || _<- [node() | nodes()]])])),
            register(idgamesHandler, spawn(?MODULE, gamesHandler, [node(), nodes(), [], []]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.


connect(Node) ->
  case net_adm:ping(Node) of
    pang -> connect(Node);
    pong -> io:format("Conectado a ~p ~n", [Node])
  end.
