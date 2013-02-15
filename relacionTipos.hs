-- Determina, para un tipo de ataque, cuales tipos son super efectivos,
-- cuales tipos son resistentes y cuales son inmunes.

relacionAtaqueTipo :: Type      -- Tipo de ataque a determinar la relación.
                   -> ( [Type]  -- Tipos super efectivos a el (2x dano). 
                      , [Type]  -- Tipos resistentes a el (0.5x dano).
                      , [Type]  -- Tipos inmunes a el (0x dano).
                      )
                      
relacionAtaqueTipo x = case x of
  Bug      -> ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
  Dark     -> ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  Dragon   -> ([Dragon], [Steel], [])
  Electric -> ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  Fighting -> ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
  Fire     -> ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  Flying   -> ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  Ghost    -> ([Ghost, Psychic], [Steel, Dark], [Normal])
  Grass    -> ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
  Ground   -> ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
  Ice      -> ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  Normal   -> ([], [Rock, Steel], [Ghost])
  Poison   -> ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  Psychic  -> ([Fighting, Poison], [Steel, Psychic], [Dark])
  Rock     -> ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  Steel    -> ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  Water    -> ([Ground, Rock, Fire], [Water, Grass, Dragon], [])
