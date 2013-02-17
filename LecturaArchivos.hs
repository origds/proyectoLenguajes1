-- Modulo donde se encuentran las funciones para la lectura de archivos

import System.Environment
import System.IO
import System.Directory
import System.Exit (exitSuccess)
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Batalla
import Pokemon

crearEspecies :: String -> [Especie]
crearEspecies csv =  map convertir $ map separar lineas
  where
    lineas = lines csv
    separar = splitOneOf ","
    
    convertir :: [String] -> Especie
    convertir [a,b,c,d,e,f,g,h,i,j,k,l] =
      Especie {catalogo = read a
              ,nombreEsp = b
              ,tipoPokemon = if (d == "") then Left (read c) else Right (read c,read d)
              ,estadEsp = Estadisticas {hp = read e
                                       ,ataque = read f
                                       ,defensa= read g
                                       ,ataqueEsp = read h
                                       ,defensaEsp = read i
                                       ,velocidad = read j}
              ,prevolucion = if (k == "") then Nothing else Just (read k)
              ,evolucion = l}

crearAtaques :: String -> [Ataque]
crearAtaques csv = map convertir $ map separar lineas
  where
    lineas = lines csv
    separar = splitOneOf ","
    
    convertir :: [String] -> Ataque
    convertir [a,b,c,d,e] =
      Ataque {nombreAt = a
              ,tipo = read b
              ,fisico = read c
              ,pp = read d
              ,poderAtaque = read e}

crearMonstruos :: String -> [Especie] -> [Ataque] -> [Monstruo]
crearMonstruos csv listaEspecies listaAtaques = map convertir $ map separar lineas
  where
    lineas = lines csv
    separar = splitOneOf ","
    
    convertir :: [String] -> Monstruo
    convertir [a,b,c,d,e,f,g]=
      Monstruo {especie = fromJust $ laEspecie (read a)
              ,sobrenombre = b
              ,nivel = read c
              ,hpactual = 0 -- El pokemon debe inicir la batalla con el maximo hp
              ,ataques = ((fromJust $ elAtaque d , pp $ fromJust $ elAtaque d), 
                          if (e == "") then Nothing else Just (fromJust $ elAtaque e, pp $ fromJust $ elAtaque e ),
                          if (f == "") then Nothing else Just (fromJust $ elAtaque f, pp $ fromJust $ elAtaque f ),
                          if (g == "") then Nothing else Just (fromJust $ elAtaque g, pp $ fromJust $ elAtaque g )) }
                          
    elAtaque :: String -> Maybe Ataque
    elAtaque s = obtenerAtaque s (mapaAtaques listaAtaques)
      where
        mapaAtaques :: [Ataque] -> M.Map String Ataque
        mapaAtaques lista = foldl' combinar M.empty lista
          where
            combinar mapaViejo ataqueNuevo = M.insert (nombreAt ataqueNuevo) ataqueNuevo mapaViejo
    
        obtenerAtaque :: String -> M.Map String Ataque -> Maybe Ataque
        obtenerAtaque nombre mapa = M.lookup nombre mapa
    
    laEspecie :: Int -> Maybe Especie
    laEspecie c = obtenerEspecie c (mapaEspecies listaEspecies)
      where
        mapaEspecies :: [Especie] -> M.Map Int Especie 
        mapaEspecies lista = foldl' combinar M.empty lista
          where
            combinar mapaViejo especieNueva = M.insert (catalogo especieNueva) especieNueva mapaViejo
    
        obtenerEspecie :: Int -> M.Map Int Especie -> Maybe Especie
        obtenerEspecie n mapa = M.lookup n mapa
        
asignarHP :: [Monstruo] -> [Monstruo]
asignarHP listaMons = map hp listaMons
  where
    hp :: Monstruo -> Monstruo
    hp m = m { hpactual = maxHp m }

main :: IO()
main = do
  [archivoEspecies, archivoAtaques, archivoEntrenador1, archivoEntrenador2] <- getArgs
  csvEspecies <- readFile archivoEspecies
  csvAtaques <- readFile archivoAtaques
  csvEntrenador1 <- readFile archivoEntrenador1
  csvEntrenador2 <- readFile archivoEntrenador2
  print ("ESPECIES")
  let listaEspecies = crearEspecies csvEspecies
  --print $ listaEspecies
  print ("ATAQUEES")
  let listaAtaques = crearAtaques csvAtaques
  --print $ listaAtaques
  print ("MONSTRUOS PARA EL ENTRENADOR 1")
  let listaEntrenador1 = crearMonstruos csvEntrenador1 listaEspecies listaAtaques
  let listaE1hp = asignarHP listaEntrenador1
  --print $ listaE1hp
  print ("MONSTRUOS PARA EL ENTRENADOR 2")
  let listaEntrenador2 = crearMonstruos csvEntrenador2 listaEspecies listaAtaques
  let listaE2hp = asignarHP listaEntrenador2
  --print $ listaE2hp
  print ("ATACAR")
  --print $ fromJust (atacar 2 ((!!) listaE1hp 1))
  print ("CAMBIAR")
  print $ fromJust (cambiar 3 listaE1hp)
  print ("INFO")
  info (listaE1hp !! 1) listaE1hp
  print ("AYUDA")
  ayuda (fromJust (cambiar 5 listaE1hp)) listaE1hp

--Flujo de la Batalla


  --Se seleccionan los dos primeros monstruos pasados en los archivos para
  --cada entrenador.
  let monstruoAct2 = fromJust (cambiar 0 listaEntrenador2)
  let monstruoAct1 = fromJust (cambiar 0 listaEntrenador1)

  let velAct1 = actualVel monstruoAct1
  let velAct2 = actualVel monstruoAct2
  let maxVel = max velAct1 velAct2

  putStrLn "Entrenador 1, elige una accion!"
  accion1 <- getLine
  putStrLn "Entrenador 1, elige una accion!"
  accion2 <- getLine

  
  case words accion1 of
    ["atacar", n] -> atacar n
    ["cambiar", n] -> fromJust (cambiar n listaEntrenador1)
