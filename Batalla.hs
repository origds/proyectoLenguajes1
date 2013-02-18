-- Modulo donde se definen las funciones para la batalla pokemon

module Batalla where 

import Control.Monad
import Data.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Pokemon

-- Funcion para escoger el ataque del pokemon en batalla

atacar :: Int -> Monstruo -> Maybe Ataque
atacar n mons
  | snd atack == 0 = Nothing
  | otherwise = Just $ fst atack
    where
      atack = (listarAtaque (ataques mons)) !! (n-1)

-- Funcion que aplica el daño a un pokemon

nuevoHp :: Monstruo -> Double -> Monstruo
nuevoHp defensor daño = 
  defensor { hpactual = if ((hpactual defensor) - (floor daño)) < 0 then 0
                                                                else (hpactual defensor) - (floor daño)}

-- Funcion que verifica que un pokemon este inconsciente

inconsciente :: Monstruo -> Bool
inconsciente mons
  | (hpactual mons) == 0 = True
  | otherwise = False

-- Funcion para cambiar de monstruo en la batalla

cambiar :: Int -> [Monstruo] -> Maybe Monstruo
cambiar n lista
  | hpactual (lista !! (n-1)) == 0 = Nothing
  | otherwise = Just (lista !! (n-1))
  
-- Funcion para obtener informacion de un monstruo 

info :: Monstruo -> String
info monstruo = "POKEMON ACTUAL \n\n" ++ imprimirInfo monstruo

-- Funcion para obtener la lista de ataques y la lista de monstruos

ayuda :: Monstruo -> [Monstruo] -> String
ayuda mons listaM = unlines
  [ "LISTA DE ATAQUES", "", imprimirAtaques (listarAtaque (ataques mons))
  ,"LISTA DE MONSTRUOS", "", imprimirMonstruos listaM ]

-- Funcion para asignar el pokemon actual al entrenador 

actualPokemon :: Entrenador -> Monstruo -> Entrenador
actualPokemon ent pok = ent { pokemon = pok }

-- Funcion para rendirse

rendirse :: Entrenador -> Entrenador
rendirse ent = ent { rendido = True }

-- Funciones auxiliares para impresion

imprimirInfo :: Monstruo -> String
imprimirInfo mons = unlines
 [ "Especie: " ++ show (especie mons), ""
 , "Sobrenombre: " ++ (sobrenombre mons), ""
 , "Nivel: " ++ show (nivel mons), ""
 , "HP actual: " ++ show (hpactual mons), ""
 , "Ataques: " ++ show (listarAtaque (ataques mons)) ]
  
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