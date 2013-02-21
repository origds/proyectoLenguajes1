{-# LANGUAGE RecordWildCards #-}

-- Modulo donde se encuentran las funciones para la lectura de archivos
import System.Environment
import System.IO
import System.Directory
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
    lineas = filter (not . null) $ lines csv
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
    lineas = filter (not . null) $ lines csv
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
    lineas = filter (not . null) $ lines csv
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
  = Atacar Int
  | Cambiar Int 
  | Rendirse
  | Info TipoInfo
  | Ayuda
  deriving (Eq, Read, Show)

data Estado
 = Estado
   { e1, e2 :: Entrenador
   , aQuienLeToca :: Jugador
   }
 deriving (Eq, Read, Show)

parsearAccion :: Entrenador -> String -> Maybe Accion
parsearAccion Entrenador {..} s =
  case words s of
    ["info", p]    -> Info <$> parsearTipoInfo p
    ["ayuda"]      -> Just Ayuda
    ["cambiar", n] -> Just $ Cambiar (read n)
    ["atacar", n]  -> Just $ Atacar  (read n)
    ["rendirse"]   -> Just Rendirse
    _ -> Nothing

elJugador :: Jugador -> Entrenador -> Entrenador -> Entrenador
elJugador Primero e _ = e
elJugador Segundo _ e = e

-- Funcion que cambia a los entrenadores de rol

cambialos :: Entrenador -> Entrenador -> Jugador -> Entrenador-> (Entrenador, Entrenador)
cambialos e1 e2 Primero eNuevo = (eNuevo, e2)
cambialos e1 e2 Segundo eNuevo = (e1, eNuevo)

elOtro Primero = Segundo
elOtro Segundo = Primero

--Recibe dos entrenadores y el numero del entrenador que quiero printear

turno :: Entrenador -> Entrenador -> Jugador -> IO ()
turno e1 e2 j = do
  putStrLn $ show j ++ " ingresa una acción!"
  line <- getLine

  let jugadorActivo = elJugador j e1 e2
  
  case parsearAccion jugadorActivo line of
    Nothing -> do
      putStrLn $ "\nLa accion indicada no es valida, ingresa otra accion " ++ show j
      turno e1 e2 j
    Just a -> case a of 
      Atacar n -> do
                    if inconsciente (pokemonActivo jugadorActivo) 
                      then do
                        putStrLn $ "Tu pokemon esta inconsciente, " ++ show j ++ ". Debes cambiarlo antes de atacar!"
                        turno e1 e2 j
                      else do 
                        let defensor = elJugador (elOtro j) e1 e2
                        case atacar n (pokemonActivo jugadorActivo) of
                          Nothing -> do
                                      putStrLn $ "\nEl ataque tiene pp 0 debes escoger otro, " ++ show j ++ "!"
                                      turno e1 e2 j
                          Just atake -> do
                                      let
                                        pokemon = pokemonActivo jugadorActivo
                                        daño = dañoAtaque pokemon (pokemonActivo defensor) atake
                                        nuevoDefensor = nuevoHp defensor daño
                                        pp = actualizarPP pokemon n
                                        ataquepp = actualizarAtaque pokemon n (listarAtaque $ ataques pokemon) pp
                                        nuevoAtacante = actualizarEntrenador ataquepp jugadorActivo n
                                        (e1Nuevo, e2Nuevo) = cambialos e1 e2 j nuevoAtacante
                                        (e1Viejo, e2Viejo) = cambialos e1 e2 (elOtro j) nuevoDefensor
                                      turno e1Nuevo e2Nuevo (elOtro j)
                                      

      Cambiar n     -> do 
                        if (activo jugadorActivo)+1 == n 
                          then do
                                putStrLn $ "Tu pokemon actual es el pokemon " ++ show n
                                turno e1 e2 j
                          else
                              case cambiarPokemon jugadorActivo n of
                                Nothing -> do
                                            putStrLn $ "\nEl pokemon esta inconsciente debes cambiarlo, " ++ show j ++ "!"
                                            turno e1 e2 j
                                Just nuevo -> do
                                            let (e1Nuevo, e2Nuevo) = cambialos e1 e2 j nuevo
                                            turno e1Nuevo e2Nuevo (elOtro j)
      Rendirse      -> do
                        let perdedor = rendirse jugadorActivo
                        if rendido perdedor then putStrLn $ "\nLa batalla ha finalizado! El " ++ show j 
                                                      ++ " se ha rendido. El ganador es el " ++ show (elOtro j) ++ "!!!"
                                            else putStrLn $ "\nLa batalla ha finalizado! El " ++ show (elOtro j)
                                                 ++ " se ha rendido. El ganador es el " ++ show j ++ "!!!"
      Info t        -> if t == Yo then do
                                        putStrLn $ info $ pokemonActivo jugadorActivo
                                        turno e1 e2 j
                                  else do
                                        putStrLn $ info $ pokemonActivo (elJugador (elOtro j) e1 e2)
                                        turno e1 e2 j
      Ayuda         -> do
                        putStrLn $ ayuda (pokemonActivo jugadorActivo) (listaPokemones jugadorActivo)
                        turno e1 e2 j
  
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
  
  putStrLn "¡¡¡BATALLA POKEMON!!!\n\n"
  
--Flujo de la Batalla

  let entrenador1 = Entrenador { activo = 0
                           , listaPokemones = listaE1hp
                           , rendido = False }
  
  let entrenador2 = Entrenador { activo = 0
                           , listaPokemones = listaE2hp
                           , rendido = False }
  
  turno entrenador1 entrenador2 Primero
  putStrLn "\n\n¡¡¡FIN DE LA BATALLA POKEMON!!!"


