{-Modulo Monad Batalla: En este modulo se definen los tipos necesarios para realizar
  la batalla pokemon haciendo uso de los Monads de Haskell.
  
  Autores: 
    Carla Urrea 09-11215
    Oriana Gomez 09-10336
    
  Fecha de Ultima Modificacion: 22/02/2013
-}

module MonadBatalla where

import Pokemon

--Definicion de tipos necesarios para la batalla
--Accion: Indica el tipo de accion que puede hacer un usuario
--        La accion Listo se utiliza para marcar una accion como ya realizada
data Accion
  = Atacar Int
  | Cambiar Int 
  | Rendirse
  | Info TipoInfo
  | Ayuda
  | Listo
  deriving (Eq, Read, Show)

--Jugador: Primero corresponde al entrenador 1
--         Sgundo corresponde a entrenador 2
data Jugador
  = Primero
  | Segundo
  deriving (Eq, Read, Bounded, Enum)

--TioInfo: Yo retorna la info acerca de los datos del entrenador que introdujo la accion
--         Rival Yo retorna la info acerca de los datos del otro entrenador que introdujo la accion
data TipoInfo
  = Yo 
  | Rival
  deriving (Show, Eq, Read, Bounded, Enum)

--Estado: Represena un estado de la batalla para un determinado momento
--Cada estado de una batalla tiene dos entrenadores, un jugador que representa al entrenador
--que esta realizando la accion y las dos acciones de cada jugador para el momento
data Estado
  = Estado
    { e1, e2 :: Entrenador
    , aQuienLeToca :: Jugador
    , a1, a2 :: Maybe Accion
    }

--Tipo Batalla: Cada batalla de tipo a esta representada por una "caja" que dado un estado 
--se obtiene un IO de ese estado junto a ese a 
newtype Batalla a = B (Estado -> IO (Estado, a))

--mandaALaGuerra: Esta funcion actua como la funcion de return para el MonadBatalla
mandaALaGuerra :: a -> Batalla a
mandaALaGuerra a = B $ \ estadoInicial -> return (estadoInicial, a)

combina :: Batalla a -> Batalla b -> Batalla b
combina (B b1) (B b2) = B $ \ estadoInicial -> do
  (estadoIntermedio, a) <- b1 estadoInicial
  (estadoFinal, b) <- b2 estadoIntermedio
  return (estadoFinal, b)

combinaConectando :: Batalla a -> (a -> Batalla b) -> Batalla b
combinaConectando (B b1) f = B $ \ estadoInicial -> do
  (estadoIntermedio, a) <- b1 estadoInicial
  let B b2 = f a
  (estadoFinal, b) <- b2 estadoIntermedio
  return (estadoFinal, b)

instance Monad Batalla where
  return = mandaALaGuerra
  (>>)   = combina
  (>>=)  = combinaConectando

dame :: Batalla Estado
dame = B $ \ estadoInicial -> return (estadoInicial, estadoInicial)

dameEl :: (Estado -> a) -> Batalla a
dameEl unaProyección = B $ \ estadoInicial -> return (estadoInicial, unaProyección estadoInicial)

toma :: Estado -> Batalla ()
toma estadoNuevo = B $ \ estadoInicial -> return (estadoNuevo, ())

tomaYDame :: (Estado -> Estado) -> Batalla ()
tomaYDame f = do 
  i <- dame
  toma $ f i

lift :: IO a -> Batalla a
lift programa = B $ \ estadoInicial -> do
  a <- programa
  return (estadoInicial, a)

correrBatalla :: Estado -> Batalla a -> IO a
correrBatalla estadoInicial (B laBatalla) = do
  (estadoFinal, a) <- laBatalla estadoInicial
  return a
