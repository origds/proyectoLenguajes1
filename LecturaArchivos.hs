{-# LANGUAGE RecordWildCards #-}
{-
  Modulo Lectura de Archivo: En este modulo se se realiza la lectura y el 
  parseo de los archivos obtenidos de la entrada estandar
  
  Autores: 
    Carla Urrea 09-11215
    Oriana Gomez 09-10336
    
  Fecha de Ultima Modificacion: 22/02/2013
-}

module LecturaArchivos where

-- Modulo donde se encuentran las funciones para la lectura de archivos
import System.IO
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord

import Batalla
import MonadBatalla
import Pokemon

--crearEspecies: Funcion que crea las especies que se encuentran en el archivo
--pasado por entrada estandar
crearEspecies :: String -> [Especie]
crearEspecies csv =  map convertir $ map separar lineas
  where
    lineas = filter (not . null) $ lines csv
    separar = splitOneOf ","
    
    convertir :: [String] -> Especie
    convertir [a,b,c,d,e,f,g,h,i,j,k,l] =
      Especie 
        {
          catalogo = read a
          , nombreEsp = b
          , tipoPokemon = 
              if (d == "") 
                then Left (read c) 
                else Right (read c,read d)
          , estadEsp = 
              Estadisticas 
              {
                hp = read e
                , ataque = read f
                , defensa= read g
                , ataqueEsp = read h
                , defensaEsp = read i
                , velocidad = read j
              }
          , prevolucion = 
              if (k == "") 
                then Nothing 
                else Just (read k)
          ,evolucion = l
        }

--crearAtaques: Funcion que crea los que se encuentran en el archivo
--pasado por entrada estandar
crearAtaques :: String -> [Ataque]
crearAtaques csv = map convertir $ map separar lineas
  where
    lineas = filter (not . null) $ lines csv
    separar = splitOneOf ","
    
    convertir :: [String] -> Ataque
    convertir [a,b,c,d,e] =
      Ataque {nombreAt = a
              ,tipo = read b
              ,fisico = read c
              ,pp = read d
              ,poderAtaque = read e}

--crearMonstruos: Funcion que crea los monstruos que se encuentran 
--en el archivo pasado por entrada estandar
crearMonstruos :: String -> [Especie] -> [Ataque] -> [Monstruo]
crearMonstruos csv listaEspecies listaAtaques = 
  map convertir $ map separar lineas
    where
      lineas = filter (not . null) $ lines csv
      separar = splitOneOf ","
      
      convertir :: [String] -> Monstruo
      convertir [a, b, c, d, e, f, g] =
        Monstruo 
          {
            especie = 
              fromJust $ laEspecie (read a)
              , sobrenombre = b
              , nivel = read c
              , hpactual = 0
              , ataques = 
                  ((fromJust $ elAtaque d , pp $ fromJust $ elAtaque d), 
                    if (e == "") 
                      then Nothing 
                      else 
                        Just (fromJust $ elAtaque e, pp $ fromJust $ elAtaque e ),
                    if (f == "") 
                      then Nothing 
                      else 
                        Just (fromJust $ elAtaque f, pp $ fromJust $ elAtaque f ),
                    if (g == "") 
                      then Nothing 
                      else
                        Just (fromJust $ elAtaque g, pp $ fromJust $ elAtaque g )) 
          }
                          
      elAtaque :: String -> Maybe Ataque
      elAtaque s = obtenerAtaque s (mapaAtaques listaAtaques)
        where
          mapaAtaques :: [Ataque] -> M.Map String Ataque
          mapaAtaques lista = foldl' combinar M.empty lista
            where
              combinar mapaViejo ataqueNuevo = 
                M.insert (nombreAt ataqueNuevo) ataqueNuevo mapaViejo
      
          obtenerAtaque :: String -> M.Map String Ataque -> Maybe Ataque
          obtenerAtaque nombre mapa = M.lookup nombre mapa
      
      laEspecie :: Int -> Maybe Especie
      laEspecie c = obtenerEspecie c (mapaEspecies listaEspecies)
        where
          mapaEspecies :: [Especie] -> M.Map Int Especie 
          mapaEspecies lista = foldl' combinar M.empty lista
            where
              combinar mapaViejo especieNueva = 
                M.insert (catalogo especieNueva) especieNueva mapaViejo
      
          obtenerEspecie :: Int -> M.Map Int Especie -> Maybe Especie
          obtenerEspecie n mapa = M.lookup n mapa
          
asignarHP :: [Monstruo] -> [Monstruo]
asignarHP listaMons = map hp listaMons
  where
    hp :: Monstruo -> Monstruo
    hp m = m { hpactual = maxHp m }
