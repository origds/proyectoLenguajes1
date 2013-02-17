-- Modulo donde se definen las funciones para la batalla pokemon

module Batalla where 

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
    elMonstruo s = obtenerMonstruo s (mapaMonstruo listaMonstruo)
      where
        mapaMonstruo :: [Monstruo] -> M.Map String Monstruo
        mapaMonstruo lista = foldl' combinar M.empty lista
          where
            combinar mapaViejo monstruoNuevo = M.insert (sobrenombre monstruoNuevo) ataqueNuevo mapaViejo
    
        obtenerMonstruo :: String -> M.Map String Monstruo -> Maybe Monstruo
        obtenerMonstruo nombre mapa = M.lookup nombre mapa
 




