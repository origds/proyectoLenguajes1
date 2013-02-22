{-# LANGUAGE RecordWildCards #-}

{-Flujo Batalla: Este es el archivo principal del programa aqui se definen
  las funciones principales y el main
  
  Autores: 
    Carla Urrea 09-11215
    Oriana Gomez 09-10336
    
  Fecha de Ultima Modificacion: 22/02/2013
-}

import System.Environment
import System.Exit
import System.IO
import System.Directory
import Control.Monad
import Data.List
import Data.Functor
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord

import Batalla
import LecturaArchivos
import MonadBatalla
import Pokemon

--pasearTipoInfo: Funcion que parsea la accion info
parsearTipoInfo s = case s of
  "yo"    -> Just Yo
  "rival" -> Just Rival
  _       -> Nothing

--parsearAccion: Funcion que parsea cualquier accion de la batalla
parsearAccion :: Entrenador -> String -> Maybe Accion
parsearAccion b1 s =
  case words s of
    ["info", p]    -> Info <$> parsearTipoInfo p
    ["ayuda"]      -> Just Ayuda
    ["cambiar", n] -> Just $ Cambiar (read n)
    ["atacar", n]  -> Just $ Atacar  (read n)
    ["rendirse"]   -> Just Rendirse
    _ -> Nothing

--elJugador: Funcion que dado un jugador retorna el entrenador correspondiente
elJugador :: Jugador -> Entrenador -> Entrenador -> Entrenador
elJugador Primero e _ = e
elJugador Segundo _ e = e

-- Funcion que cambia a los entrenadores de rol
elOtro Primero = Segundo
elOtro Segundo = Primero

--finBatalla: Funcion que indica que una batalla termino
finBatalla :: Jugador -> Batalla ()
finBatalla j = do
  ent1 <- dameEl e1
  ent2 <- dameEl e2
  accion1 <- dameEl a1
  accion2 <- dameEl a2
  case j of
    Primero -> do
      let inconscientes = and $ map inconsciente (listaPokemones ent1)
      case inconscientes of
        True -> do
          lift $ putStrLn $ "\nEl Entrenador 1 ha perdido!, todos "
                            ++ "sus pokemones estan inconscientes."
                            ++ "\nEl ganador es el Entrenador 2!!!"
          fin
        False -> return ()

    Segundo -> do
      let inconscientes = and $ map inconsciente (listaPokemones ent2)
      case inconscientes of
        True -> do
          lift $ putStrLn $ "\nEl Entrenador 2 ha perdido!, todos "
                            ++ "sus pokemones estan inconscientes."
                            ++ "\nEl ganador es el Entrenador 1!!!"
          fin
        False -> return ()

-- Funciones auxiliares para print
fin :: Batalla()
fin = lift $ do
  putStrLn "\n\n¡¡¡FIN DE LA BATALLA POKEMON!!!"           
  exitSuccess

--cambiar: Funcion para cambiar el pokemon actual
cambiar :: Jugador -> Int -> Batalla ()
cambiar j n = do
  ent <- dameEl (obtenerJugador j)
  estadoInicial <- dameEl id
  if (n > length (listaPokemones ent)) || (n<0)
    then do
      lift $ putStrLn $ "Este pokemon no existe " ++ show j 
                       ++ " , ejecuta ayuda para mas informacion"
      resetearAccion j
      turno
    else do      
      case activo ent + 1 == n of
        True -> do
          lift $ putStrLn $ "Tu pokemon actual es el pokemon " ++ show n
          case j of
            Primero -> do
              tomaYDame $ \ estado -> estado { a1 = Nothing }
              turno
            Segundo -> do
              tomaYDame $ \ estado -> estado { a2 = Nothing }
              turno
        False ->
          case cambiarPokemon ent n of
            Nothing -> do
              lift $ putStrLn $ "\nEl pokemon esta inconsciente debes "
                                ++ "cambiarlo, " ++ show j ++ "!"
              resetearAccion j
            Just nuevo -> do
              tomaYDame $ \ estado -> case j of
                Primero -> estado { e1 = nuevo }
                Segundo -> estado { e2 = nuevo }
                    
-- atacar: Funcion para atacar a un pokemon
atacar :: Jugador -> Int -> Batalla()
atacar j n= do
  estado <- dameEl id
  let
    (ent1,ent2) = case j of
      Primero -> (e1 estado, e2 estado)
      Segundo -> (e2 estado, e1 estado)
  
  case inconsciente $ pokemonActivo ent2 of
    True -> do
      lift $ putStrLn $ "\nEl pokemon tiene hpactual 0!! Debes "
                        ++ "escoger otro pokemon " ++ show (elOtro j) ++ "!\n"
      resetearAccion $ elOtro j
      turno  
    False -> 
      case inconsciente $ pokemonActivo ent1 of
        True -> do
          lift $ putStrLn $ "\nEl pokemon tiene hpactual 0!! Debes "
                            ++ "escoger otro pokemon " ++ show j ++ "!\n"
          resetearAccion j
          turno   
        False -> do
          let 
            pokemon = pokemonActivo ent1
            listaA = listarAtaques $ ataques pokemon
          if (n > length listaA) || (n<0)
            then do
              lift $ putStrLn $ "Este ataque no existe " ++ show j 
                                ++ " , ejecuta ayuda para mas informacion"
              resetearAccion j
              turno
            else
              case atacarPokemon n (pokemonActivo ent1) of
                Nothing -> do
                  lift $ putStrLn $ "\nEl ataque tiene pp 0 debes escoger otro, "
                                    ++ show j ++ "!\n"
                  resetearAccion j
                  turno 
                Just atake -> do
                  let
                    pokemon2 = pokemonActivo ent2
                    daño = dañoAtaque pokemon pokemon2 atake
                    nuevoDefensor = nuevoHp ent2 daño
                  lift $ putStrLn $ "\nEl pokemon " ++ sobrenombre pokemon 
                                    ++ " ataco al pokemon " ++ sobrenombre pokemon2 
                                    ++ " con " ++ nombreAt atake
                                    ++ " causandole " ++ show (floor daño) 
                                    ++ " de daño!!!\n"  
                  case inconsciente $ pokemonActivo nuevoDefensor of
                      True -> do
                        lift $ putStrLn $ "\nEl ataque fue super efectivo!!\n"
                        tomaYDame $ \ estado -> case (elOtro j) of
                          Primero -> 
                            estado 
                              { e2 = actualizarPP ent1 (n-1)
                              , e1 = nuevoDefensor
                              , a1 = Nothing
                              , a2 = Nothing 
                              }
                          Segundo -> 
                            estado 
                              { e1 = actualizarPP ent1 (n-1)
                              , e2 = nuevoDefensor
                              , a1 = Nothing
                              , a2 = Nothing 
                              }
                        finBatalla $ elOtro j
                        lift $ putStrLn $ "\nDebes escoger otro pokemon " 
                                          ++ show (elOtro j) ++ "!\n"
                        turno
                      False -> 
                        tomaYDame $ \ estado -> case j of
                          Primero -> 
                            estado 
                              { e1 = actualizarPP ent1 (n-1)
                              , e2 = nuevoDefensor
                              , a1 = Just Listo 
                              }                
                          Segundo -> 
                            estado 
                              { e2 = actualizarPP ent1 (n-1)
                              , e1 = nuevoDefensor
                              , a2 = Just Listo 
                              }   
                              
--hpcero: Funcion auxiliar para imprimir por consola
hpcero :: Jugador -> Batalla ()
hpcero j = 
  lift $ putStrLn $ "\nEl pokemon tiene hpactual 0!! Debes "
                    ++ "escoger otro pokemon " ++ show j ++ "!\n"

--resetearAccion: Funcion que permite asignar nothing a una accion cuando hay algun error
resetearAccion :: Jugador -> Batalla()
resetearAccion j = do
  tomaYDame $ \ estado -> 
    case j of
      Primero -> estado { a1 = Nothing }
      Segundo -> estado { a2 = Nothing }
  
--turno: Funcion que maneja el flujo de la batalla
turno :: Batalla ()
turno = do
  estadoInicial <- dameEl id
  aQuienLeTocaba <- dameEl aQuienLeToca
  ent1 <- dameEl e1
  ent2 <- dameEl e2

  case (a1 estadoInicial, a2 estadoInicial) of
    (Just Ayuda, _) -> do
      lift $ putStrLn $ ayuda (pokemonActivo ent1) (listaPokemones ent1)
      tomaYDame $ \ estado -> estado { a1 = Nothing }
    (_, Just Ayuda) -> do
      lift $ putStrLn $ ayuda (pokemonActivo ent2) (listaPokemones ent2)
      tomaYDame $ \ estado -> estado { a2 = Nothing }
    (Just (Info t), _) ->
      case t of
          Yo -> do
            lift $ putStrLn $ info $ pokemonActivo ent1
            tomaYDame $ \ estado -> estado { a1 = Nothing }
          Rival -> do
            lift $ putStrLn $ info $ pokemonActivo ent2
            tomaYDame $ \ estado -> estado { a1 = Nothing }
    (_, Just (Info t)) ->
      case t of
          Yo -> do
            lift $ putStrLn $ info $ pokemonActivo ent2
            tomaYDame $ \ estado -> estado { a2 = Nothing }
          Rival -> do
            lift $ putStrLn $ info $ pokemonActivo ent1
            tomaYDame $ \ estado -> estado { a2 = Nothing }
    (Just a1', Just a2') -> do
      case (a1', a2') of
        (Rendirse, Rendirse) -> do
          lift $ putStrLn "\nHubo un empate!!!"
          fin
        (Rendirse, _) -> do
          lift $ putStrLn $ "\nLa batalla ha finalizado! El Entrenador 1 se ha "
                            ++ "rendido. El ganador es el Entrenador 2!!!"
          fin
        (_, Rendirse) -> do
          lift $ putStrLn $ "\nLa batalla ha finalizado! El Entrenador 2 se ha "
                            ++ "rendido. El ganador es el Entrenador 1!!!"
          fin
        (Cambiar n, _) ->  do           
          cambiar Primero n
          tomaYDame $ \ estado -> estado { a1 = Just Listo }
        (_, Cambiar n) -> do
          cambiar Segundo n
          tomaYDame $ \ estado -> estado { a2 = Just Listo }
        (Atacar n, Atacar m) -> do
          let
            dameElEntrenador Primero = e1 estadoInicial
            dameElEntrenador Segundo = e2 estadoInicial
          let 
            maxVel = 
              maximumBy (comparing $ actualVel . pokemonActivo . dameElEntrenador) [Primero,Segundo]
          case maxVel of
            Primero -> 
              case inconsciente $ pokemonActivo ent1 of
                True -> do 
                  hpcero Primero
                  tomaYDame $ \ estado -> estado { a1 = Nothing }
                  turno   
                False -> do
                  atacar Primero n
                  atacar Segundo m
            Segundo -> 
              case inconsciente $ pokemonActivo ent2 of
                True -> do 
                  hpcero Segundo
                  tomaYDame $ \ estado -> estado { a2 = Nothing }
                  turno   
                False -> do
                  atacar Segundo m
                  atacar Primero n
        (Atacar n, _) -> atacar Primero n
        (_, Atacar n) -> atacar Segundo n
        (Listo, Listo) -> do
          tomaYDame $ \ estado -> estado { a1 = Nothing, a2 = Nothing }
    (Nothing, _) -> do
      lift $ putStrLn $ "Entrenador 1 ingresa una accion!"
      accion <- lift getLine
      case parsearAccion ent1 accion of
        Nothing -> lift $ putStrLn $ "\nLa accion indicada no es valida, ingresa otra accion Entrenador 1\n"
        Just a -> tomaYDame $ \ estado -> estado { a1 = Just a }
    (_, Nothing) -> do
      lift $ putStrLn $ "Entrenador 2 ingresa una accion!"
      accion <- lift getLine
      case parsearAccion ent2 accion of
        Nothing -> lift $ putStrLn $ "\nLa accion indicada no es valida, ingresa otra accion Entrenador 2\n"
        Just a -> tomaYDame $ \ estado -> estado { a2 = Just a }   
  turno    
      

-- main: Funcion principal del programa
main :: IO()
main = do
  [archivoEspecies, archivoAtaques, archivoEntrenador1, archivoEntrenador2] <- getArgs
  csvEspecies <- readFile archivoEspecies
  csvAtaques <- readFile archivoAtaques
  csvEntrenador1 <- readFile archivoEntrenador1
  csvEntrenador2 <- readFile archivoEntrenador2
  
  -- Se crea la lista de especies
  let 
    listaEspecies = crearEspecies csvEspecies 
    -- Se crea la lista de ataques
    listaAtaques = crearAtaques csvAtaques    
    -- Se crea la lista del entrenador1
    listaEntrenador1 = crearMonstruos csvEntrenador1 listaEspecies listaAtaques 
    -- Se asignan los maxHP a los monstruos del entrenador1
    listaE1hp = asignarHP listaEntrenador1 
    -- Se crea la lista del entrenador2
    listaEntrenador2 = crearMonstruos csvEntrenador2 listaEspecies listaAtaques
    -- Se asignan los maxHP a los monstruos del entrenador2
    listaE2hp = asignarHP listaEntrenador2 
  
  putStrLn $ 
    unlines 
      [""
      ,"¡¡¡QUE COMIENCE LA BATALLA POKEMON!!!"
      ,""
      ,""
      , "Acciones: "
      ,""
      , "1. atacar n: n indica el numero del ataque a usar."
      , "2. cambiar n: n indica el numero del pokemon a cambiar"
      , "3. info yo | rival: da la informacion del entrenador o de su rival"
      , "4. ayuda: muestra los ataques del pokemon actual y los pokemones del entrenador"
      , "5. rendirse"
      ]

--Flujo de la Batalla

  let 
    entrenador1 = Entrenador
      { activo = 0
      , listaPokemones = listaE1hp
      , rendido = False
      }
  
    entrenador2 = Entrenador 
      { activo = 0
      , listaPokemones = listaE2hp
      , rendido = False 
      }
                           
    estadoInicial  = Estado
      { e1 = entrenador1
      , e2 = entrenador2
      , aQuienLeToca = Primero
      , a1 = Nothing
      , a2 = Nothing }
                      
  correrBatalla estadoInicial turno