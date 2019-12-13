# TRABAJO PRACTICO SO1
## Franco García Cillero G-5238/8
## Dante Noguera N-1160/6

## Overview
Cuando un cliente se conecta al servidor __dispatcher__ lanza __psocket__ que 
se encarga de parsear los comandos que ingrese el cliente, preguntar a __pbalance__ 
por el nodo con menos carga y lanzar __pcomando__ en dicho nodo (el criterio para
determinar el nodo con menos carga es la cantidad de puertos de erlang abiertos 
que tiene el nodo esto indica la cantidad de clientes conectados al nodo/servidor).
Además lanza __sendUpdate__ que envía actualizaciones del servidor al usuario. 
__pcomando__ ejecuta los comandos en el servidor, __usersManager__ se encarga de 
los comandos relacionados con registro de usuarios, __gamesManager__ se encarga 
de los comandos relacionados con los juegos.

## Decisiones tomadas
* Un usuario se puede registrar una única vez. Si intenta registrarse de nuevo
será bloqueado.
* Un usuario no puede jugar consigo mismo.
* Cuando un usuario crea una partida no puede jugar hasta que haya un contrincante.
* Cuando una partida termina se resetea.
* Una partida se puede abandonar en cualquier momento, la misma se resetea.
* Un usuario no puede observar más de una vez una partida.
* Un jugador no puede observar la partida en la que juega.

