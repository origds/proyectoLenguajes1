-- Modulo donde se definen las funciones para la batalla pokemon

module Batalla where 

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
      atack = (listarAtaque (ataques mons)) !! n 
 
-- Funcion para cambiar de monstruo en la batalla

cambiar :: Int -> [Monstruo] -> Maybe Monstruo
cambiar n lista
  | hpactual (lista !! n) == 0 = Nothing
  | otherwise = Just (lista !! n)
  
-- Funcion para obtener informacion de un monstruo 

info :: String -> [Monstruo] -> Monstruo
info s listaEntrenador = fromJust $ elMonstruo s
  where
    elMonstruo :: String -> Maybe Monstruo
    elMonstruo s = obtenerMonstruo s (mapaMonstruo listaEntrenador)
      where
        mapaMonstruo :: [Monstruo] -> M.Map String Monstruo
        mapaMonstruo lista = foldl' combinar M.empty lista
          where
            combinar mapaViejo monstruoNuevo = M.insert (sobrenombre monstruoNuevo) monstruoNuevo mapaViejo
    
        obtenerMonstruo :: String -> M.Map String Monstruo -> Maybe Monstruo
        obtenerMonstruo nombre mapa = M.lookup nombre mapa
        
-- Funcion para obtener la lista de ataques y la lista de monstruos

ayuda :: Monstruo -> IO()
ayuda mons = do
  print ("LISTA DE ATAQUES")
  let listaA = listarAtaque (ataques mons)
  let tamañoA = length listaA
  imprimirAtaques tamañoA listaA
--   print ("LISTA DE MONSTRUOS")
--   let tamañoM = length listaM
--   imprimirMonstruos tamañoM listaM

imprimirAtaques :: Int -> [(Ataque,Int)]-> IO()
imprimirAtaques n lista
  | n == 1 = do
      print ("Ataque 0")
      print $ nombreAt (fst (lista !! 0))
      print ("PP actual")
      print $ snd (lista !! 0)
  | n == 2 = do
      print ("Ataque 0")
      print $ nombreAt (fst (lista !! 0))
      print ("PP actual")
      print $ snd (lista !! 0)
      print ("Ataque 1")
      print $ nombreAt (fst (lista !! 1))
      print ("PP actual")
      print $ snd (lista !! 1)
  | n == 3 = do
      print ("Ataque 0")
      print $ nombreAt (fst (lista !! 0))
      print ("PP actual")
      print $ snd (lista !! 0)
      print ("Ataque 1")
      print $ nombreAt (fst (lista !! 1))
      print ("PP actual")
      print $ snd (lista !! 1)
      print ("Ataque 2")
      print $ nombreAt (fst (lista !! 2))
      print ("PP actual")
      print $ snd (lista !! 2)
  | otherwise = do
      print ("Ataque 0")
      print $ nombreAt (fst (lista !! 0))
      print ("PP actual")
      print $ snd (lista !! 0)
      print ("Ataque 1")
      print $ nombreAt (fst (lista !! 1))
      print ("PP actual")
      print $ snd (lista !! 1)
      print ("Ataque 2")
      print $ nombreAt (fst (lista !! 2))
      print ("PP actual")
      print $ snd (lista !! 2)
      print ("Ataque 3")
      print $ nombreAt (fst (lista !! 3))
      print ("PP actual")
      print $ snd (lista !! 3)
      
-- imprimirMonstruos :: Int -> [Monstruo]-> IO()
-- imprimirMonstruo n lista
--   | n == 1 = do
--       if (hpactual (lista !! 0)) /= 0 then print ("Pokemon 0")
--                                            print $ sobrenombre (lista !! 0)
--   | n == 2 = do
--       if (hpactual (lista !! 0)) /= 0 then do
--                                            print ("Pokemon 0")
--                                            print $ sobrenombre (lista !! 0)
--       
--       if (hpactual (lista !! 1)) /= 0 then do
--                                            print ("Pokemon 1")
--                                            print $ sobrenombre (lista !! 1)
--   | n == 3 = do
--       if (hpactual (lista !! 0)) /= 0 then do
--                                            print ("Pokemon 0")
--                                            print $ sobrenombre (lista !! 0)
--       
--       if (hpactual (lista !! 1)) /= 0 then do
--                                            print ("Pokemon 1")
--                                            print $ sobrenombre (lista !! 1)
--                                            
--       if (hpactual (lista !! 2)) /= 0 then do
--                                            print ("Pokemon 2")
--                                            print $ sobrenombre (lista !! 2)
--   | otherwise = do
--       if (hpactual (lista !! 0)) /= 0 then do
--                                            print ("Pokemon 0")
--                                            print $ sobrenombre (lista !! 0)
--       
--       if (hpactual (lista !! 1)) /= 0 then do
--                                            print ("Pokemon 1")
--                                            print $ sobrenombre (lista !! 1)
--       
--       if (hpactual (lista !! 0)) /= 0 then do
--                                            print ("Pokemon 2")
--                                            print $ sobrenombre (lista !! 2)
--       
--       if (hpactual (lista !! 1)) /= 0 then do
--                                            print ("Pokemon 3")
--                                            print $ sobrenombre (lista !! 3)
-- 
-- printMonstruo :: Int -> [Monstruo] -> IO()
-- printMonstruo n lista = do
--   print ("Pokemon n")
--   print $ sobrenombre (lista !! 2)