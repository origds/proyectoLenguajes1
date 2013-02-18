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

--Flujo de la Batalla

turnoEntrenador1 :: IO(String)
turnoEntrenador1 = do
  putStrLn "Entrenador 1, elige una accion!"
  getLine

turnoEntrenador2 :: IO(String)
turnoEntrenador2 = do 
  putStrLn "Entrenador 2, elige una accion!" 
  getLine

batalla :: Monstruo -> Monstruo -> [Monstruo] -> [Monstruo] -> IO(String) -> IO(String)-> IO()
batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp a1 a2 = do

  accion1 <- a1
  accion2 <- a2
  let acciones = accion1 ++ " " ++ accion2

  case words acciones of
    --Si ambos entrenadores solicitan un cambio
    --se realiza el cambio y se continua al siguiente turno
    ["cambiar", n, "cambiar", m] -> do
      if inconsciente (fromJust (cambiar (read n) listaE1hp))
        then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 1!"
                batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 a2
        else do
          let monstruoActCambiado1 = fromJust (cambiar (read n) listaE1hp) 
          if inconsciente (fromJust (cambiar (read m) listaE2hp))
            then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 2!"
                    batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp a1 turnoEntrenador2
            else do
              let monstruoActCambiado2 = fromJust (cambiar (read m) listaE2hp)
              batalla monstruoActCambiado1 monstruoActCambiado2 listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2
    
    --Si alguno de los dos entrenadores solicita un cambio
    --el mismmo se realiza y luego se realiza el ataque.
    ["cambiar", n, "atacar", m] -> do

      let a = atacar (read m) monstruoAct2
      --Si el ataque retorna Nothing, se muestra un mensaje indicando que
      --ese ataque ya no puede ser usado. Si no, se realiza el cambio realizado
      --por el entrenador 1 y Se calcula el dano realizado
      --al pokemon defensor y se verifica que el mismo no haya quedado inconsiente
      if (isNothing a) 
        then do putStrLn "No tienes PP para este ataque!. Debes elegir otro ataque, Entrenador 2!"
                batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp a1 turnoEntrenador2    
        else  do 
          if inconsciente (fromJust (cambiar (read n) listaE1hp))
            then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 1!"
                    batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 a2
            else do
              let monstruoActCambiado = fromJust (cambiar (read m) listaE1hp)
              let daño = dañoAtaque monstruoAct2 monstruoActCambiado (fromJust(a))
              let monstruo1NuevoHp = nuevoHp monstruoActCambiado daño
      
          --Si el pokemon quedo inconsciente el entrenador 1 debe realizar un 
          --cambio para continua con la batalla, si no continuo el flujo norma
          --del juego
              if inconsciente monstruo1NuevoHp
                then do 
                  putStrLn "El pokemon esta inconsciente!. Debes cambiarlo, Entrenador 1"
                  batalla monstruo1NuevoHp monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2 
                else 
                  batalla monstruo1NuevoHp monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2
    
      --Si alguno de los dos entrenadores solicita un cambio
      --el mismmo se realiza y luego se realiza el ataque.
    ["atacar", n, "cambiar", m] -> do

      let a = atacar (read n) monstruoAct1
      --Si el ataque retorna Nothing, se muestra un mensaje indicando que
      --ese ataque ya no puede ser usado. Si no, se realiza el cambio realizado
      --por el entrenador 1 y Se calcula el dano realizado
      --al pokemon defensor y se verifica que el mismo no haya quedado inconsiente
      if (isNothing a) 
        then do putStrLn "No tienes PP para este ataque!. Debes elegir otro ataque, Entrenador 1"
                batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 a2
        else  do 
          if inconsciente (fromJust (cambiar (read m) listaE2hp))
            then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 2!"
                    batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp a1 turnoEntrenador2
            else do
              let monstruoActCambiado = fromJust (cambiar (read m) listaE2hp)
              let daño = dañoAtaque monstruoAct1 monstruoActCambiado (fromJust(a))
              print "DANO!!!!"
              print daño
              let monstruo2NuevoHp = nuevoHp monstruoActCambiado daño
          --Si el pokemon quedo inconsciente el entrenador 1 debe realizar un 
          --cambio para continua con la batalla, si no continuo el flujo norma
          --del juego
              if inconsciente monstruo2NuevoHp
                then do 
                  putStrLn "El pokemon esta inconsciente!. Debes cambiarlo, Entrenador 2!"
                  batalla monstruoAct1 monstruo2NuevoHp listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2 
                else 
                  batalla monstruoAct1 monstruo2NuevoHp listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2

    ["info", "yo", "info", "yo"] -> do 
      putStrLn $ info monstruoAct1
      putStrLn $ info monstruoAct2
      batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2

--Esto es lo que debe pasar si ambos entrenadores atacan!

  --let velAct1 = actualVel monstruoAct1
  --let velAct2 = actualVel monstruoAct2
  --let maxVel = max velAct1 velAct2

  --putStrLn "Entrenador 1, elige una accion!"
  --accion1 <- getLine
  --putStrLn "Entrenador 2, elige una accion!"
  --accion2 <- getLine

  --Se verifica cual pokemon tiene mayor velocidad para determinar
  --quien realiza su accion primero
  --if (maxVel == velAct1) 
  --  then
  --    case words accion1 of
  --      ["atacar", n] -> print (fromJust (atacar (read n) monstruoAct1))
  --      ["cambiar", n] -> print (fromJust (cambiar (read n) listaE1hp))
  --      --["info", "yo"] -> info listaEntrenador1 "yo" 
  --      --["info", "rival"] -> info listaEntrenador2 "rival"
  --      ["ayuda"] -> do ayuda monstruoAct1
  --                  batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp
  --      _ -> do putStrLn ("Say what!?")
  --              batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp
  --else if (maxVel == velAct2) 
  --  then
  --    case whatords accion1 of
  --      ["atacar", n] -> print (fromJust (atacar (read n) monstruoAct2))
  --      ["cambiar", n] -> print (fromJust (cambiar (read n) listaE2hp))
  --      --["info", "yo"] -> info listaEntrenador1 "yo"
  --      --["info", "rival"] -> info listaEntrenador2 "rival"
  --      ["ayuda"] -> ayuda monstruoAct1
  --      _ -> putStrLn ("Say what!?")
  --else putStr "SHUT THE FUCK OFF."
  
main :: IO()
main = do
  [archivoEspecies, archivoAtaques, archivoEntrenador1, archivoEntrenador2] <- getArgs
  csvEspecies <- readFile archivoEspecies
  csvAtaques <- readFile archivoAtaques
  csvEntrenador1 <- readFile archivoEntrenador1
  csvEntrenador2 <- readFile archivoEntrenador2
  --print ("ESPECIES")
  let listaEspecies = crearEspecies csvEspecies
  --print $ listaEspecies
  --print ("ATAQUEES")
  let listaAtaques = crearAtaques csvAtaques
  --print $ listaAtaques
  --print ("MONSTRUOS PARA EL ENTRENADOR 1")
  let listaEntrenador1 = crearMonstruos csvEntrenador1 listaEspecies listaAtaques
  let listaE1hp = asignarHP listaEntrenador1
  --print $ listaE1hp
  --print ("MONSTRUOS PARA EL ENTRENADOR 2")
  let listaEntrenador2 = crearMonstruos csvEntrenador2 listaEspecies listaAtaques
  let listaE2hp = asignarHP listaEntrenador2
  --print $ listaE2hp
  --print ("ATACAR")
  --print $ fromJust (atacar 2 ((!!) listaE1hp 1))
  --print ("CAMBIAR")
  --print $ fromJust (cambiar 3 listaE1hp)
  print ("INFO")
  putStrLn $ info (listaE1hp !! 1)
  print ("AYUDA")
  putStrLn $ ayuda (fromJust (cambiar 3 listaE1hp)) listaE1hp

--Flujo de la Batalla

  --Valores iniciales de la batalla
  --Se seleccionan los dos primeros monstruos pasados en los archivos para
  --cada entrenador.
  let monstruoAct1 = fromJust (cambiar 1 listaE1hp)
  let monstruoAct2 = fromJust (cambiar 1 listaE2hp)
  
  batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoEntrenador1 turnoEntrenador2
  print("")


