-module(tp).
-export([init/0, dispatcher/1, psocket/1, pstat/0, pbalance/1, pcomando/2]).
-import ('aux', [findminload/1, updateloadlist/2]).
-define(SERVERS, ['nodoA@127.0.0.0']).
-define(LOADS, [0]).

%Proposito: % Funcion que calcula la carga del nodo.
load() ->
    length(erlang:ports()).

%Proposito: cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.

%Funciones:
%- gen_tcp:accept(ListenSocket) acepta un request de conexion que viene del LisenSocket devuelve
%{ok, Socket}
%{error, closed} si listen socket se cerro

%Observaciones:
%-control de error en gen_tcp:accept(ListenSocket)

dispatcher(ListenSocket, UserNameList) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nuevo cliente: ~p ~n", [Socket]),
            spawn(?MODULE, psocket, [Socket, UserNameList]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    end,
    receive
		{ok, NewUserName} -> dispatcher(ListenSocket, [NewUserName | UserNameList]),
        {invalid_username} -> dispatcher(ListenSocket, UserNameList)
    end.
%Proposito: por cada pedido, psocket crea un nuevo proceso (pcomando) que realiza todo cálculo necesario y
%le devuelva una respuesta a psocket, que le enviará al cliente. Además pcomando
%se encargará de generar los mensajes correspondientes para el resto de los clientes
%y mandarlos a sus respectivos psocket, de ser necesario.

%Funciones :
%- gen_tcp:recv(Socket, 0). Recive un paquete de un socket, 0 indica que pude ser de longitud arbitraria,
%devuelve ok con el paquete o error

%Observaciones:
%-control de error en gen_tcp:recv(Socket, 0)
psocket(Socket, UserNameList) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            io:format("Nuevo comando!"),
            idpbalance ! {self(), where},
            receive
                {Node, _} -> io:format("Recibido! ~p ~n", [Node]), spawn(Node, ?MODULE, pcomando, [binary_to_list(Cmd), self(), UserNameList]) % Crea el pcomando en el servidor correspondiente
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end.
    %rtas pcomando
    %receive
    %    {con, ok} -> io:format("ok");
    %    {con, error} -> ok
    %end.

pcomando(Cmd, PidPsocket, UserNameList) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
         ["CON", UserName] -> if 
                                lists:member(UserName, UserNameList) -> ;
                                true -> 
                                   
         ["LSG", ] -> ok;
         ["NEW", ] -> ok;
         ["ACC", ] -> ok;
         ["PLA", ] -> ok;
         ["LEA", ] -> ok;
         ["BYE", ] -> ok;
         ["UPD", ] -> ok
    end
         
         
    
    
    
%Proposito: recibe informacion de pstat, indica a psocket en que nodo crear pcomando. Primero
% envia a pcomando despues actualiza la lista.
pbalance(LoadList) ->
    receive
        {PidPsocket, where} -> PidPsocket ! lists:nth(1, lists:keysort(2, LoadList)), pbalance(LoadList);
        {Node, Load} -> pbalance([{X,case X of Node -> Load; _ -> Y end} || {X,Y} <- LoadList])
    end.

%Proposito: envia la carga de los nodos a pbalance
pstat() ->
    [{idpbalance, Node} ! {node(), load()} || Node <- [node() | nodes()]],
    timer:sleep(1000),
    pstat().

%Proposito: crea el ListenSocket e inicializa pstat, pbalance

%Funciones:
% -get_tcp:listen(0, [{active, false}]), crea el LisenSocket 0 indica que el sistema se encargará
% de buscar un puerto disponible, [{active, false}] es una opcion que ....

%Observaciones:
% se verifica que gen_tcp:listen no tire error.

 init() ->
    case gen_tcp:listen(8091, [{active, false}, binary]) of
        {ok, ListenSocket} ->
            register(iddispatcher, spawn(?MODULE, dispatcher, [ListenSocket, []])),
            spawn(?MODULE, pstat, []),
            register(idpbalance, spawn(?MODULE, pbalance, [lists:zip(?SERVERS, ?LOADS)]));
        {error, Msg} -> io:format("Error: ~p al crear ListenSocket~n", [Msg])
    end.
