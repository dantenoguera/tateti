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

        {error, closed} ->
            io:format("Error en el cliente ~p. Conexion cerrada.~n", [Socket]);



%Proposito: recibe informacion de pstat, indica a psocket en que nodo crear pcomando
pbalance() ->
    ok


%Proposito: envia la carga de los nodos a pbalance
pstat() ->
    ok
