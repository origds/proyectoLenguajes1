{-# LANGUAGE RecordWildCards #-}

-- Modulo donde se encuentran las funciones para la lectura de archivos
import System.Environment
import System.IO
import System.Directory
import System.Exit (exitSuccess)
import Control.Monad
import Data.List
import Data.Functor
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
data Jugador
  = Primero
  | Segundo
  deriving (Eq, Read, Bounded, Enum)

instance Show Jugador where
  show Primero = "Entrenador 1"
  show Segundo = "Entrenador 2"

data TipoInfo
  = Yo 
  | Rival
  deriving (Show, Eq, Read, Bounded, Enum)

parsearTipoInfo s = case s of
  "yo"    -> Just Yo
  "rival" -> Just Rival
  _       -> Nothing

data Accion
  = Atacar Ataque
  | Cambiar Int 
  | Rendirse
  | Info TipoInfo
  | Ayuda
  deriving (Eq, Read, Show)

parsearAccion :: Entrenador -> String -> Maybe Accion
parsearAccion Entrenador {..} s =
  case words s of
    ["info", p]    -> Info <$> parsearTipoInfo p
    ["ayuda"]      -> Just Ayuda
    ["cambiar", n] -> Just $ Cambiar (read n)
    ["atacar", n]  -> Atacar <$> atacar (read n) ( pokemonActivo Entrenador {..})
    ["rendirse"]   -> Just Rendirse
    _ -> Nothing

elJugador :: Jugador -> Entrenador -> Entrenador -> Entrenador
elJugador Primero e _ = e
elJugador Segundo _ e = e

elOtro Primero = Segundo
elOtro Segundo = Primero

cambialos :: Entrenador -> Entrenador -> Jugador -> Entrenador-> (Entrenador, Entrenador)
cambialos e1 e2 Primero eNuevo = (eNuevo, e2)
cambialos e1 e2 Segundo eNuevo = (e1, eNuevo)

cambiarPokemon :: Entrenador -> Int -> Entrenador
cambiarPokemon e n = e { activo = n }

pokemonActivo Entrenador {..} = listaPokemones !! activo

--Recibe dos entrenadores y el numero del entrenador que quiero printear
turno :: Entrenador -> Entrenador -> Jugador -> IO ()
turno e1 e2 j = do
  putStrLn $ show j ++ " ingresa una accion!"
  line <- getLine

  let jugadorActivo = elJugador j e1 e2
  case parsearAccion jugadorActivo line of
    Nothing -> do
      putStrLn "Callate"
      turno e1 e2 j
    Just a -> case a of 
      Atacar ataque -> undefined
      Cambiar n     -> do 
                        let
                          nuevo = cambiarPokemon jugadorActivo n
                          (e1Nuevo, e2Nuevo) = cambialos e1 e2 j nuevo
                        turno e1Nuevo e2Nuevo (elOtro j)
      Rendirse      -> undefined
      Info t        -> undefined
      Ayuda         -> undefined



--batalla :: Entrenador -> Entrenador -> IO(String) -> IO(String)-> IO()
--batalla entrenador1 entrenador2 a1 a2 = do
--  accion1 <- a1
--  accion2 <- a2

--  let acciones = accion1 ++ " " ++ accion2

--  case words acciones of
--    --Si ambos entrenadores solicitan un cambio
--    --se realiza el cambio y se continua al siguiente turno
--    ["cambiar", n, "cambiar", m] -> do

      --if inconsciente $ fromJust $ cambiar (read n) (listaPokemones entrenador1)
      --  then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 1!"
      --          let turno1 = turno entrenador1 entrenador2 Primero
      --          batalla entrenador1 entrenador2 turno1 a2
      --  else do
      --    let monstruoCambiado1 = fromJust $ cambiar (read n) (listaPokemones entrenador1)
      --    let entrenador1Modif = actualPokemon entrenador1 monstruoCambiado1
          
      --    if inconsciente $ fromJust $ cambiar (read m) (listaPokemones entrenador2)
      --      then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 2!"
      --              let turno2 = turno entrenador1 entrenador2 Segundo
      --              batalla entrenador1 entrenador2 a1 turno2
      --      else do
      --        let monstruoCambiado2 = fromJust (cambiar (read m) (listaPokemones entrenador2))
      --        let entrenador2Modif = actualPokemon entrenador2 monstruoCambiado2
      --        let turno1 = turno entrenador1 entrenador Primero
      --        let turno2 = turno entrenador1 entrenador2 Segundo
      --        batalla entrenador1Modif entrenador2Modif turno1 turno2
    ----Si alguno de los dos entrenadores solicita un cambio
    ----el mismmo se realiza y luego se realiza el ataque.
    --["cambiar", n, "atacar", m] -> do

    --  --Si el pokemon del entrenador 2 esta inconsciente
    --  if inconsciente $ pokemon entrenador2
    --    then do
    --      putStrLn "El pokemon esta inconsciente! Debes cambiarlo, Entrenador 2!" 
    --      let turno2 = turnoE2 entrenador1 entrenador2
    --      batalla entrenador1 entrenador2 a1 turno2
    --    else do 
    --      --Si el pokemon del entrenador 2 no esta insconsciente, se verifica si el ataque tiene
    --      --PP suficiente para atacar. Si no tiene PP, debe elegir otro ataque. Si tiene PP, se realiza
    --      --el cambio solicitador por el entrenador1 (si el pokemon no esta inconsciente) 
    --      let a = atacar (read m) (pokemon entrenador2)
    --      if (isNothing a) 
    --        then do putStrLn "No tienes PP para este ataque!. Debes elegir otro ataque, Entrenador 2!"
    --                let turno2 = turnoE2 entrenador1 entrenador2
    --                batalla entrenador1 entrenador2 a1 turno2
    --        else  do 
    --          if inconsciente $ fromJust $ cambiar (read n) (listaPokemones entrenador1)
    --            then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 1!"
    --                    let turno1 = turnoE1 entrenador1 entrenador2
    --                    batalla entrenador1 entrenador2 turno1 a2
    --            else do
    --              --Cambio de Pokemon
    --              let monstruoCambiado = fromJust $ cambiar (read n) (listaPokemones entrenador1)
    --              --Modifico al Entrenador talque su nuevo pokemon actual sea el que elegi en el cambio
    --              let entrenador1Modif = actualPokemon entrenador1 monstruoCambiado
    --              --Calculo el dano del ataque
    --              let daño = dañoAtaque (pokemon entrenador2) (pokemon entrenador1Modif) (fromJust(a))
    --              --Calculo el nuevoHp del monstruo que fue atacado
    --              let monstruo1NuevoHp = nuevoHp (pokemon entrenador1Modif) daño
    --              --Modifico al entrenador tal que su pokemon actual sea el monstruo cambiado con su nuevoHp
    --              let entrenador1Atacado = actualPokemon entrenador1Modif monstruo1NuevoHp
    --              if inconsciente $ pokemon entrenador1Atacado
    --                then do 
    --                  putStrLn "El pokemon esta inconsciente!. Debes cambiarlo, Entrenador 1"
    --                  let turno1 = turnoE1 entrenador1 entrenador2
    --                  let turno2 = turnoE2 entrenador1 entrenador2
    --                  batalla entrenador1Atacado entrenador2 turno1 turno2

    --                else do
    --                  let turno1 = turnoE1 entrenador1 entrenador2
    --                  let turno2 = turnoE2 entrenador1 entrenador2 
    --                  batalla entrenador1Atacado entrenador2 turno1 turno2
          
    ----Si alguno de los dos entrenadores solicita un cambio
    ----el mismmo se realiza y luego se realiza el ataque.
    --["atacar", n, "cambiar", m] -> do

    --   if inconsciente (pokemon entrenador1)
    --    then do
    --      putStrLn "El pokemon esta inconsciente! Debes cambiarlo, Entrenador 1!"
    --      batalla entrenador1 entrenador2 (turnoE1 entrenador1 entrenador2) a2
    --    else do
    --      let a = atacar (read n) (pokemon entrenador1)
    --      --Si el ataque retorna Nothing, se muestra un mensaje indicando que
    --      --ese ataque ya no puede ser usado. Si no, se realiza el cambio realizado
    --      --por el entrenador 1 y Se calcula el dano realizado
    --      --al pokemon defensor y se verifica que el mismo no haya quedado inconsiente
    --      if (isNothing a) 
    --        then do putStrLn "No tienes PP para este ataque!. Debes elegir otro ataque, Entrenador 1"
    --                let turno1 = turnoE1 entrenador1 entrenador2
    --                batalla entrenador1 entrenador2 turno1 a2
    --        else  do 
    --          if inconsciente (fromJust (cambiar (read m) (listaPokemones entrenador2)))
    --            then do putStrLn "El pokemon esta inconsciente!. Debes elegir otro, Entrenador 2!"
    --                    let turno2 = turnoE2 entrenador1 entrenador2
    --                    batalla entrenador1 entrenador2 a1 turno2
    --            else do
    --              --Cambio de Pokemon
    --              let monstruoCambiado = fromJust $ cambiar (read m) (listaPokemones entrenador2)
    --              --Modifico al Entrenador talque su nuevo pokemon actual sea el que elegi en el cambio
    --              let entrenador2Modif = actualPokemon entrenador2 monstruoCambiado
    --              --Calculo el dano del ataque
    --              let daño = dañoAtaque (pokemon entrenador1) (pokemon entrenador2Modif) (fromJust(a))
    --              --Calculo el nuevoHp del monstruo que fue atacado
    --              let monstruo2NuevoHp = nuevoHp (pokemon entrenador2Modif) daño
    --              --Modifico al entrenador tal que su pokemon actual sea el monstruo cambiado con su nuevoHp
    --              let entrenador2Atacado = actualPokemon entrenador2Modif monstruo2NuevoHp
    --          --Si el pokemon quedo inconsciente el entrenador 1 debe realizar un 
    --          --cambio para continua con la batalla, si no continuo el flujo norma
    --          --del juego
    --              if inconsciente monstruo2NuevoHp
    --                then do 
    --                  putStrLn "El pokemon esta inconsciente!. Debes cambiarlo, Entrenador 2!"
    --                  let turno1 = turnoE1 entrenador1 entrenador2
    --                  let turno2 = turnoE2 entrenador1 entrenador2
    --                  batalla entrenador1 entrenador2Atacado turno1 turno2
    --                else do
    --                  let turno1 = turnoE1 entrenador1 entrenador2
    --                  let turno2 = turnoE2 entrenador1 entrenador2                      
    --                  batalla entrenador1 entrenador2Atacado turno1 turno2



    --["info", "yo", "info", "yo"] -> do 
    --  putStrLn $ info (pokemon entrenador1)
    --  putStrLn $ info (pokemon entrenador2)
    --  let turno1 = turno entrenador1 entrenador2 Primero
    --  let turno2 = turno entrenador1 entrenador2 Segundo
    --  batalla entrenador1 entrenador2 turno1 turno2
    
    --["atacar",n,"atacar",m] -> do
    --  if inconsciente monstruoAct1
    --    then do
    --      putStrLn "El pokemon esta inconsciente! Debes cambiarlo, Entrenador 1!"
    --      batalla monstruoAct1 monstruoAct2 listaE1hp listaE2hp turnoE1 a2
    --    else do
    --      if inconsciente 
    --      let a = atacar (read n) monstruoAct1
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
  let listaEspecies = crearEspecies csvEspecies -- Se crea la lista de especies
  let listaAtaques = crearAtaques csvAtaques    -- Se crea la lista de ataques
  let listaEntrenador1 = crearMonstruos csvEntrenador1 listaEspecies listaAtaques -- Se crea la lista del entrenador1
  let listaE1hp = asignarHP listaEntrenador1 -- Se asignan los maxHP a los monstruos del entrenador1
  let listaEntrenador2 = crearMonstruos csvEntrenador2 listaEspecies listaAtaques -- Se crea la lista del entrenador2
  let listaE2hp = asignarHP listaEntrenador2 -- Se asignan los maxHP a los monstruos del entrenador2
  print ("INFO")
  putStrLn $ info (listaE1hp !! 1)
  print ("AYUDA")
  putStrLn $ ayuda (fromJust (cambiar 3 listaE1hp)) listaE1hp

--Flujo de la Batalla

  let entrenador1 = Entrenador { activo = 0
                           , listaPokemones = listaE1hp
                           , rendido = False }
  
  let entrenador2 = Entrenador { activo = 0
                           , listaPokemones = listaE2hp
                           , rendido = False }
  
  turno entrenador1 entrenador2 Primero
  
  --batalla entrenador1 entrenador2 turno1 turno2
  print("")


