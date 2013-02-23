{-# LANGUAGE RecordWildCards #-}

{-Modulo Batalla: En este modulo se definen las funciones auxiliares para la batalla
  pokemon
  
  Autores: 
    Carla Urrea 09-11215
    Oriana Gomez 09-10336
    
  Fecha de Ultima Modificacion: 22/02/2013
-}
module Batalla where 

import Data.Data
import Data.List
import Data.Maybe

import MonadBatalla
import Pokemon

-- atacarPokemon: Funcion para escoger el ataque del pokemon en batalla       
atacarPokemon :: Int -> Monstruo -> Maybe Ataque
atacarPokemon n mons
  | snd atack == 0 = Nothing
  | otherwise = Just $ fst atack
    where
      atack = (listarAtaques $ ataques mons) !! (n-1)

--obtenerJugador: Dado unugador y un estado se retorna el jugador actual para ese estado
obtenerJugador :: Jugador -> Estado -> Entrenador 
obtenerJugador j = 
  case j of 
    Primero -> e1 
    Segundo -> e2
    
--actualizarPP: Funcion que actualiza el PP del ataque de un pokemon una vez que este
--fue utilizado.
actualizarPP :: Entrenador -> Int -> Entrenador
actualizarPP e n = e { listaPokemones = listaPokemones' }
  where
    listaPokemones' = insertar pokemon' (listaPokemones e) (activo e)
    pokemon = listaPokemones e !! activo e
    pokemon' = pokemon { ataques = ataques' }
    listaAtaques = listarAtaques $ ataques pokemon
    (ataque, pp) = listaAtaques !! n
    ataque' = (ataque, pp-1)
    lista' = insertar ataque' listaAtaques n
    tuplificar4 (a:b:c:d:_) = (fromJust a, b, c, d)
    ataques' = tuplificar4 $ map Just lista' ++ repeat Nothing

-- insertar: Funcion que inserta un elemento en la posicion n de una lista.
insertar :: Show a => a -> [a] -> Int -> [a]
--insertar _ [] _ = []
insertar e (_:xs) 0 = e : xs
insertar e (x:xs) n = x : insertar e xs (n - 1)
insertar a b c = error $ show (a, b, c)

--nuevoHp Funcion que aplica el daño a un pokemon
nuevoHp :: Entrenador -> Double -> Entrenador
nuevoHp defensor daño = defensor { listaPokemones = listanuevo }
  where
    nuevoPokemon = (pokemonActivo defensor) 
        { 
          hpactual = max 0 $ (hpactual $ pokemonActivo defensor) - (floor daño) 
        }
    listanuevo   = insertar nuevoPokemon (listaPokemones defensor) $ activo defensor
  
--inconsciente: Funcion que verifica que un pokemon este inconsciente
inconsciente :: Monstruo -> Bool
inconsciente mons
  | (hpactual mons) == 0 = True
  | otherwise = False

--obtenerPokemon: Funcion para el monstruo a cambiar en la batalla
obtenerPokemon :: Int -> [Monstruo] -> Maybe Monstruo
obtenerPokemon n lista
  | hpactual (lista !! (n-1)) == 0 = Nothing
  | otherwise = Just (lista !! (n-1))
  
--cambiarPokemon: Funcion que cambia el pokemon activo de un entrenador
cambiarPokemon :: Entrenador -> Int -> Maybe Entrenador
cambiarPokemon e n =
  if inconsciente pokemon
    then Nothing
    else Just e { activo = n - 1 }
  where 
    pokemon = fromJust $ obtenerPokemon n $ listaPokemones e

--pokemonActivo: Funcion que retorna el pokemon actual de la lista 
--de pokemones de un entrenador
pokemonActivo Entrenador {..} = listaPokemones !! activo

--info: Funcion para obtener informacion de un monstruo 
info :: Monstruo -> String
info monstruo = "\n\nPOKEMON ACTUAL \n\n" ++ imprimirInfo monstruo

--ayuda: Funcion para obtener la lista de ataques y la lista de monstruos
ayuda :: Monstruo -> [Monstruo] -> String
ayuda mons listaM = unlines
  [ "\n\nLISTA DE ATAQUES", "", imprimirAtaques (listarAtaques (ataques mons))
  ,"LISTA DE MONSTRUOS", "", imprimirMonstruos listaM ]

--rendirse: Funcion para rendirse
rendirse :: Batalla()
rendirse = do
  entrenador <- dameEl aQuienLeToca
  estadoInicial <- dameEl id
  case entrenador of
      Primero -> do
        ent1 <- dameEl e1
        let estadoNuevo = estadoInicial { e1 = ent1 { rendido = True } }
        toma $ estadoNuevo
      Segundo -> do
        ent2 <- dameEl e2
        let estadoNuevo = estadoInicial { e2 = ent2 { rendido = True } }
        toma $ estadoNuevo

-- Funciones auxiliares para impresion

imprimirInfo :: Monstruo -> String
imprimirInfo mons = unlines
 [ "Especie: " ++ show (especie mons), ""
 , "Sobrenombre: " ++ (sobrenombre mons), ""
 , "Nivel: " ++ show (nivel mons), ""
 , "HP actual: " ++ show (hpactual mons), ""
 , "Ataques: " ++ show (listarAtaques (ataques mons)) ]
  
imprimirAtaques :: [(Ataque, Int)] -> String
imprimirAtaques lista = unlines . intersperse "" $ zipWith printAtaque [0..] lista
      
imprimirMonstruos :: [Monstruo] -> String
imprimirMonstruos lista = concat . intersperse "" $ zipWith funcion [0..] lista
  where
    funcion :: Int -> Monstruo -> String
    funcion n monstruo = 
      if (hpactual monstruo) /= 0 then printMonstruo n monstruo
                                  else printError n  
       
printAtaque :: Int -> (Ataque,Int) -> String
printAtaque n (p,s) = "Ataque " ++ show (n+1) 
                      ++ ": " ++ nombreAt p 
                      ++ ". PP actual: " ++ show s
  
printMonstruo :: Int -> Monstruo -> String
printMonstruo n mons = 
  "Pokemon " ++ show (n+1)
  ++ " -> Especie: " ++ nombreEsp (especie mons) 
  ++ ". Sobrenombre: " ++ sobrenombre mons
  ++ ". HP actual: " ++ show (hpactual mons)
  ++ "\n\n"
  
printError :: Int -> String
printError n = "El pokemon " ++ show (n+1) 
               ++ " esta inconsciente"
               ++ "\n\n"