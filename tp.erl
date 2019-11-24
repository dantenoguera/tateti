-module(tp).
-export([init/0, dispatcher/1, psocket/2, pstat/0, pbalance/1, pcomando/3, userlisthandler/1]).
-import ('aux', [findminload/1, updateloadlist/2]).
-define(SERVERS, ['nodoA@127.0.0.1']).
-define(LOADS, [0]).

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
        {con, error, UserName} -> gen_tcp:send(Socket, "ERROR 0 " ++ UserName ++ "\n"), psocket(Socket, unnamed)
    end.

pcomando(Cmd, PidPsocket, User) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", UserName] -> iduserlisthandler ! {UserName, self()},
                              receive
                                  ok -> PidPsocket ! {con, ok, UserName};
                                  error -> PidPsocket ! {con, error, UserName}
                              end;
         ["LSG"] -> ok;
         ["NEW", _] -> ok;
         ["ACC"] -> ok;
         ["PLA"] -> ok;
         ["LEA"] -> ok;
         ["BYE"] -> ok;
         ["UPD"] -> ok
    end.



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

 init() ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(8091, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket])),
            spawn(?MODULE, pstat, []),
            register(iduserlisthandler, spawn(?MODULE, userlisthandler, [[]])),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip(?SERVERS, ?LOADS)]));

        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.
