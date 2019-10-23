-module(tp).
-export([init/0]).
-import (aux, [findminload/1, updateloadlist/2]).
%-define(SERVERS, ['nodo1@127.0.0.1','nodo2@127.0.0.1']).
-define(LOADS, [999, 999]).

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

dispatcher(ListenSocket) ->
    %case gen_tcp:accept(ListenSocket) of
    %    {ok, Socket} ->
    %        io:format("Nuevo cliente: ~p.~n", [Socket]);
    %        %spawn(?MODULE, psocket, [Socket]);
    %    {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket])
    %end,
    {ok, Socket} = gen_tcp:accept(ListenSocket).
    %dispatcher(ListenSocket).

%Proposito: por cada pedido, psocket crea un nuevo proceso (pcomando) que realiza todo cálculo necesario y
%le devuelva una respuesta a psocket, que le enviará al cliente. Además pcomando
%se encargará de generar los mensajes correspondientes para el resto de los clientes
%y mandarlos a sus respectivos psocket, de ser necesario.

%Funciones :
%- gen_tcp:recv(Socket, 0). Recive un paquete de un socket, 0 indica que pude ser de longitud arbitraria,
%devuelve ok con el paquete o error

%Observaciones:
%-control de error en gen_tcp:recv(Socket, 0)
psocket(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Cmd} ->
            idpbalance ! {self(), where},
            receive
                {Node} -> spawn(Node, ?MODULE, pcomando, [Cmd, self()]) % Crea el pcomando en el servidor correspondiente
            end;
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket])
    end,
    %rtas pcomando
    receive
        {con, ok} -> io:format("ok");
        {con, error} -> ok
    end.

pcomado(Cmd, PidPsocket) ->
    case string:tokens(string:strip(string:strip(Cmd, right, $\n),right,$\r), " ") of
        ["CON", Username] -> PidPsocket ! {con, ok}
    end.


%Proposito: recibe informacion de pstat, indica a psocket en que nodo crear pcomando. Primero
% envia a pcomando despues actualiza la lista.

pbalance(LoadList) ->
    receive
        {PidPsocket, where} -> PidPsocket ! findminload(LoadList), pbalance(LoadList);
        {Node, Load} -> pbalance(updateloadlist(LoadList, {Node, Load}))
    end.



%Proposito: envia la carga de los nodos a pbalance
pstat() ->
    [{idpbalance, Node} ! {node(), load()} || Node <- [node() | nodes()]],
    timer:sleep(10000),
    pstat().




%Proposito: crea el ListenSocket e inicializa pstat, pbalance

%Funciones:
% -get_tcp:listen(0, [{active, false}]), crea el LisenSocket 0 indica que el sistema se encargará
% de buscar un puerto disponible, [{active, false}] es una opcion que ....

%Observaciones:
% se verifica que gen_tcp:listen no tire error.

 init() ->
     case gen_tcp:listen(0, [{active, false}]) of
         {ok, ListenSocket} ->
            spawn(?MODULE, dispatcher, [ListenSocket]);
            %spawn(?MODULE, pstat, []),
            %register(idpbalance, spawn(?MODULE, pbalance, [lists:zip(?SERVERS, ?LOADS)]));
         {error, _} -> io:format("Error al crear ListenSocket~n")
     end.
