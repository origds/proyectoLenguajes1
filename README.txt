Universidad Simon Bolivar
Deparamento de Computacion y Tecnologia de la Informacion
Laboratorio de Lenguajes I CI-3661

Proyecto 1. Haskell

Autores.
  Carla Urrea 09-11215
  Oriana Gomez 09-10336

El proyecto funciona en su totalidad permitiendo completar las acciones solicitadas por 
los entrenadores de manera de poder realizar la batalla de manera correcta.
La implementacion del mismo fue realizada mediante el uso de Monads y su definicion se 
encuentra en el archivo MonadBatalla.hs 

En el archivo Pokemon.hs se enceuentran todas las definiciones relacionadas direcamente
con los pokemones. Al mismo tiempo es alli donde se definen las funciones que se encargan
de calcular las estadisticas actuales de los pokemones.

En el archivo Batalla.hs se definen las funciones que haran referencia a los comandos 
introducidos por los entrenadores en el terminal, ademas de otras funciones auxiliares
que ayudan a modularizar el archivo.

En el archivo LecturaArchivos.hs se realizar el parseo y la creacion de las especies, los
ataques y los entrenadores que son pasados como argumento en entrada estandar.
El archivo FlujoBatalla se definen los comandos del juego como tal usando las funciones
definidas en el archivo Batalla.hs asi como tambien las funciones que definen el turno del
jugador actual y ejecutan el comando seleccionado por el.