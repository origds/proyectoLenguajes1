{-# LANGUAGE RecordWildCards #-}

{-Modulo Pokemon: En este modulo se definen las funciones y tipos necesarios para
  la creacion de pokemones y sus entrenadores.
  
  Autores: 
    Carla Urrea 09-11215
    Oriana Gomez 09-10336
    
  Fecha de Ultima Modificacion: 22/02/2013
-}

module Pokemon where

import Data.List
import Data.Maybe

-- Se enumeran los 17 tipos de pokemon

data Tipo
  = Bug
  | Dark
  | Dragon
  | Electric
  | Fighting
  | Fire
  | Flying
  | Ghost
  | Grass
  | Ground
  | Ice
  | Normal
  | Poison
  | Psychic
  | Rock
  | Steel
  | Water
  deriving (Bounded, Eq, Enum, Read, Show)
  
-- Funcion que establece la relacion entre los tipos de ataques y los tipos Pokemon

data Relacion = Relacion { efectivos, debiles, inmunes :: [Tipo] }

relacionAtaqueTipo :: Tipo -> Relacion
relacionAtaqueTipo x = case x of
  Bug      -> Relacion [Grass, Psychic, Dark] [Fighting, Flying, Poison, Ghost, Steel, Fire] []
  Dark     -> Relacion [Ghost, Psychic] [Fighting, Steel, Dark] []
  Dragon   -> Relacion [Dragon] [Steel] []
  Electric -> Relacion [Flying, Water] [Grass, Electric, Dragon] [Ground]
  Fighting -> Relacion [Normal, Rock, Steel, Ice, Dark] [Flying, Poison, Bug, Psychic] [Ghost]
  Fire     -> Relacion [Bug, Steel, Grass, Ice] [Rock, Fire, Water, Dragon] []
  Flying   -> Relacion [Fighting, Bug, Grass] [Rock,Steel, Electric] []
  Ghost    -> Relacion [Ghost, Psychic] [Steel, Dark] [Normal]
  Grass    -> Relacion [Ground, Rock, Water] [Flying, Poison, Bug, Steel, Fire, Grass, Dragon] []
  Ground   -> Relacion [Poison, Rock, Steel, Fire, Electric] [Bug, Grass] [Flying]
  Ice      -> Relacion [Flying, Ground, Grass, Dragon] [Steel,Fire, Water] []
  Normal   -> Relacion [] [Rock, Steel] [Ghost]
  Poison   -> Relacion [Grass] [Poison, Ground, Rock, Ghost] [Steel]
  Psychic  -> Relacion [Fighting, Poison] [Steel, Psychic] [Dark]
  Rock     -> Relacion [Flying, Bug, Fire, Ice] [Fighting, Ground, Steel] []
  Steel    -> Relacion [Rock, Ice] [Steel, Fire, Water, Electric] []
  Water    -> Relacion [Ground, Rock, Fire] [Water, Grass, Dragon] []

-- Se define el tipo Estadisticas, Especie, Ataque, Monstruo y Entrenador

data Estadisticas = Estadisticas  {
  hp :: Int,
  ataque :: Int,
  defensa :: Int,
  ataqueEsp :: Int,
  defensaEsp :: Int,
  velocidad :: Int
}
  deriving (Eq, Read, Show)

data Especie = Especie  {
  catalogo :: Int,
  nombreEsp :: String,
  tipoPokemon :: Either Tipo (Tipo,Tipo),
  estadEsp :: Estadisticas,
  prevolucion :: Maybe Int,
  evolucion :: String
}
  deriving (Eq, Read, Show)

data Ataque = Ataque  {
  nombreAt :: String,
  tipo:: Tipo,
  fisico :: Bool,
  pp :: Int,
  poderAtaque :: Int
}
  deriving (Eq, Read, Show)

data Monstruo = Monstruo  {
  especie :: Especie,
  sobrenombre :: String,
  nivel :: Int,
  hpactual :: Int,
  ataques :: ((Ataque,Int),Maybe (Ataque,Int),Maybe (Ataque,Int),Maybe (Ataque,Int))
}
  deriving (Eq, Read, Show)

data Entrenador = Entrenador {
  activo :: Int,
  listaPokemones :: [Monstruo],
  rendido :: Bool
}
  deriving (Eq, Read, Show)

--Funciones base a definir para obtener las estadisticas actuales

--maxHp: permite obtener el maximo hp de un monstruo.
maxHp :: Monstruo -> Int
maxHp (Monstruo {..}) = 
  (div ((31 + (2*hp(estadEsp especie)) + div 255 4 + 100)*nivel) 100) + 10

--actualAt: permite obtener el ataque actual de un monstruo.
actualAt :: Monstruo -> Int
actualAt (Monstruo {..}) = 
  (div ((31 + 2*ataque(estadEsp especie) + div 255 4)*nivel) 100) + 5

--actualDef: permite obtener el maximo hp de un monstruo.
actualDef :: Monstruo -> Int
actualDef (Monstruo {..}) = 
  (div ((31 + 2*defensa(estadEsp especie) + div 255 4)*nivel) 100) + 5

actualAtE :: Monstruo -> Int
actualAtE (Monstruo {..}) = 
  (div ((31 + 2*ataqueEsp(estadEsp especie) + div 255 4)*nivel) 100) + 5

actualDefE :: Monstruo -> Int
actualDefE (Monstruo {..}) = 
  (div ((31 + 2*defensaEsp(estadEsp especie) + div 255 4)*nivel) 100) + 5
  
--actualVel: permite obtener la velocidad actual de un pokemon
actualVel :: Monstruo -> Int
actualVel (Monstruo {..}) = 
  (div ((31 + 2*velocidad(estadEsp especie) + div 255 4)*nivel) 100) + 5
  
-- Funciones auxiliares

--obtenerTipo: Funcion que permite obtener los tipos de un pokemon usar
obtenerTipo :: Eq a => Either a (a,a) -> [a]
obtenerTipo (Right(a,b)) =  nub [a,b]
obtenerTipo (Left a) = [a]

--listarAtaques: Funcion que permite obtener los ataques de un monstruo 
--(usar length y (!!) para obtener valores (a,b) y luego fst y snd)
listarAtaques :: ((a,b),Maybe(a,b),Maybe(a,b),Maybe(a,b)) -> [(a,b)]
listarAtaques ((a,b),a2, a3, a4) = (a,b) : catMaybes [a2,a3,a4]

--danoAtaque: Funcion a definir para calcular el daño de un ataque 

(∈) = elem

dañoAtaque :: Monstruo -> Monstruo -> Ataque -> Double
dañoAtaque atacante defensor ataq = 
  daño (nivel atacante) (fromIntegral $ poderAtaque ataq) (ataque') (defensa') modificador
    where
      (ataque', defensa') = 
         if fisico ataq 
            then ((actualAt atacante), (actualDef defensor))
            else ((actualAtE atacante), (actualDefE defensor))

      daño nivel poder ataque defensa modificador = 
        ((nivelAjustado*poder*proporcion / 50) + 2)* modificador
        where
          proporcion = fromIntegral ataque / fromIntegral defensa
          nivelAjustado = 
            (fromIntegral nivel * fromIntegral 2 / fromIntegral 5) + fromIntegral 2

      modificador :: Double
      modificador = modifAtacante * modifDefensor

      tipoAtaque :: Tipo
      tipoAtaque = tipo ataq

      tiposAtacante, tiposDefensor :: [Tipo]
      tiposAtacante = proyectarTipo atacante
      tiposDefensor = proyectarTipo defensor

      Relacion {..} = relacionAtaqueTipo tipoAtaque

      proyectarTipo = obtenerTipo . tipoPokemon . especie

      modifAtacante, modifDefensor :: Double
      modifAtacante 
        | tipoAtaque ∈ tiposAtacante = 1.5
        | otherwise                  = 1.0
      modifDefensor = product
        $  [2.0 | t <- tiposDefensor, t ∈ efectivos]
        ++ [0.5 | t <- tiposDefensor, t ∈ debiles  ]
        ++ [0.0 | t <- tiposDefensor, t ∈ inmunes  ]
  