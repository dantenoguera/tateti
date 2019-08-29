-module(tp)

%Proposito: cuando un cliente se conecta crea un nuevo hilo (psocket) que atenderá todos los pedidos de ese cliente.

%Funciones:
%- gen_tcp:accept(ListenSocket) acepta un request de conexion que viene del LisenSocket devuelve
%{ok, Socket}
%{error, closed} si listen socket se cerro

%Observaciones:
%-control de error en gen_tcp:accept(ListenSocket)

dispatcher(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nuevo cliente: ~p.~n", [Socket]),
            spawn(?MODULE, psocket, [Socket]);
        {error, closed} -> io:format("Error ~p cerrado ~n", [ListenSocket]),
    dispatcher(ListenSocket).

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
            pbalance ! {where, self()};
            receive
                {Node} -> %!!!!
        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket]);



%Proposito: recibe informacion de pstat, indica a psocket en que nodo crear pcomando
pbalance() ->
    ok


%Proposito: % Funcion que calcula la carga del nodo.


load() ->
    length(erlang:ports()).

%Proposito: envia la carga de los nodos a pbalance

pstat() ->
    idpbalance ! [{node(), } || Node <- [node() | nodes()]]
    timer:sleep(10000),
    pstat().

pcomado() ->
    ok

%Proposito: crea el ListenSocket e inicializa pstat, pbalance

%Funciones:
% -get_tcp:listen(0, [{active, false}]), crea el LisenSocket 0 indica que el sistema se encargará
% de buscar un puerto disponible, [{active, false}] es una opcion que ....

%Observaciones:
% se verifica que gen_tcp:listen no tire error.

 init() ->
     case get_tcp:listen(0, [{active, false}]) of
         {ok, ListenSocket} -> ok;
         {error, _} -> io:format("Error al crear ListenSocket~n")
     end,
     spawn(?MODULE, dispatcher, [ListenSocket]),
     spawn(?MODULE, pstat, []),
     register(idpbalance, spawn(?MODULE, pbalance, [])).
