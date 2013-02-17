-- Modulo donde se definen las funciones para la batalla pokemon

module Batalla where 

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

info :: Monstruo -> [Monstruo] -> IO()
info monstruo listaEntrenador = imprimirInfo monstruo

-- Funcion para obtener la lista de ataques y la lista de monstruos

ayuda :: Monstruo -> [Monstruo] -> IO()
ayuda mons listaM = do
  putStrLn ("LISTA DE ATAQUES")
  let listaA = listarAtaque (ataques mons)
  let tamañoA = length listaA
  imprimirAtaques tamañoA listaA
  putStrLn ("LISTA DE MONSTRUOS")
  let tamañoM = length listaM
  imprimirMonstruos tamañoM listaM

-- Funciones auxiliares para impresion

imprimirInfo :: Monstruo -> IO()
imprimirInfo mons = do
  putStrLn ("Especie: " ++ show (especie mons))
  putStrLn ""
  putStrLn ("Sobrenombre: " ++ (sobrenombre mons))
  putStrLn ""
  putStrLn ("Nivel: " ++ show (nivel mons))
  putStrLn ""
  putStrLn ("HP actual: " ++ show (hpactual mons))
  putStrLn ""
  putStrLn ("Ataques: " ++ show (listarAtaque (ataques mons)))
  putStrLn ""
  
imprimirAtaques :: Int -> [(Ataque,Int)]-> IO()
imprimirAtaques n lista
  | n == 1 = do
      printAtaque 0 lista
  | n == 2 = do
      printAtaque 0 lista
      printAtaque 1 lista
  | n == 3 = do
      printAtaque 0 lista
      printAtaque 1 lista
      printAtaque 2 lista
  | otherwise = do
      printAtaque 0 lista
      printAtaque 1 lista
      printAtaque 2 lista
      printAtaque 3 lista
      
imprimirMonstruos :: Int -> [Monstruo]-> IO()
imprimirMonstruos n lista
  | n == 1 =
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
  | n == 2 = do
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 1 lista
                                      else printError 1
  | n == 3 = do
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 1 lista
                                      else printError 1
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 2 lista
                                      else printError 2
  | n == 4 = do
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 1 lista
                                      else printError 1
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 2 lista
                                      else printError 2
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 3 lista
                                      else printError 3
  | n == 5 = do
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 1 lista
                                      else printError 1
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 2 lista
                                      else printError 2
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 3 lista
                                      else printError 3
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 4 lista
                                      else printError 4
 | otherwise = do
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 0 lista
                                      else printError 0
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 1 lista
                                      else printError 1
      if (hpactual (lista !! 0)) /= 0 then printMonstruo 2 lista
                                      else printError 2
      if (hpactual (lista !! 1)) /= 0 then printMonstruo 3 lista
                                      else printError 3
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 4 lista
                                      else printError 4
      if (hpactual (lista !! 2)) /= 0 then printMonstruo 5 lista
                                      else printError 5

printAtaque :: Int ->  [(Ataque,Int)]-> IO()
printAtaque n lista = do
  putStrLn ("Ataque " ++ show (n+1) ++ ": " ++ nombreAt (fst (lista !! n)) ++ ". PP actual: " ++ show (snd (lista !! n)))
  putStrLn ""
  
printMonstruo :: Int -> [Monstruo] -> IO()
printMonstruo n lista = do
  putStrLn $ "Pokemon " ++ show (n+1) ++ "-> " ++ "Especie: " ++ nombreEsp (especie (lista !! n)) ++ ". Sobrenombre: " ++ sobrenombre (lista !! n) ++ ". HP actual: " ++ show (hpactual (lista !! n))
  putStrLn ""
  
printError :: Int -> IO()
printError n = putStrLn $ "El pokemon " ++ show (n+1) ++ " esta inconsciente"