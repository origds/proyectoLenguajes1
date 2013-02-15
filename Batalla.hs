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

cambiar :: Int -> [Monstruo] -> Monstruo
cambiar n lista = lista !! n



