-- Declarar los data que se van a usar
data Planta = Planta
  { vida :: Int,
    soles :: Int,
    ataque :: Int
  }
  deriving (Show)

data Zombie = Zombie
  { nombre :: String,
    accesorios :: [String],
    mordiscos :: Int
  }
  deriving (Show)

data LineaDeDefensa = LineaDeDefensa
  { plantas :: [Planta],
    zombies :: [Zombie]
  }
  deriving (Show)

-- lineas de defensa de ejemplo
linea1 =
  LineaDeDefensa
    { plantas = [sunFlower, sunFlower, sunFlower],
      zombies = []
    }

linea2 =
  LineaDeDefensa
    { plantas = [peaShooter, peaShooter, sunFlower, nut],
      zombies = [zombieBase, newspaperZombie]
    }

linea3 =
  LineaDeDefensa
    { plantas = [sunFlower, peaShooter],
      zombies = [gargantuar, zombieBase, zombieBase]
    }

linea4 =
  LineaDeDefensa
    { plantas = [sunFlower, peaShooter, superPeaShooter, superSunFlower, nut, superPeaShooter],
      zombies = [gargantuar, zombieBase, zombieBase]
    }

-- 1)
-- A) Modelado de las plantas
peaShooter = Planta 5 0 2

repeater = Planta 5 0 4

sunFlower = Planta 7 1 0

nut = Planta 100 0 0

superPeaShooter = Planta 5 0 7

superSunFlower = Planta 7 2 0

-- B) Modelado de los zombies y funcion nivelDeMuerte
zombieBase = Zombie "Zombie Base" [] 1

balloonZombie = Zombie "Balloon Zombie" ["Globo"] 1

newspaperZombie = Zombie "Newspaper Zombie" ["Diario"] 2

gargantuar = Zombie "Gargantuar Hulk Smash Puny God" ["Poste Electrico", "Zombie Enano"] 30

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte = length . filter (/= ' ') . nombre

-- 2)
-- A)Especialidades de las plantas
especialidadPlanta :: Planta -> String
especialidadPlanta planta
  | soles planta > 0 = "Proveedora"
  | ataque planta > vida planta = "Atacante"
  | otherwise = "Defensiva"

-- B)Peligrosidad de los zombies
esPeligroso :: Zombie -> Bool
esPeligroso zombie = nivelDeMuerte zombie > 10 || length (accesorios zombie) > 1

-- 3)
-- A)Agregar plantas y zombies a una linea sin repetir codigo

agregarPlanta :: Planta -> LineaDeDefensa -> LineaDeDefensa
agregarPlanta planta linea = linea {plantas = plantas linea ++ [planta]}

agregarZombie :: Zombie -> LineaDeDefensa -> LineaDeDefensa
agregarZombie zombie linea = linea {zombies = zombies linea ++ [zombie]}

-- B)Saber si una linea esta en peligro
totalAtaquePlantas :: LineaDeDefensa -> Int
totalAtaquePlantas linea = sum (map ataque (plantas linea))

totalMordiscosZombies :: LineaDeDefensa -> Int
totalMordiscosZombies linea = sum (map mordiscos (zombies linea))

estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (totalAtaquePlantas linea < totalMordiscosZombies linea) || (all esPeligroso (zombies linea) && not (null (zombies linea)))

-- C)Saber si una linea necesita ser defendida
esProveedora :: Planta -> Bool
esProveedora planta = especialidadPlanta planta == "Proveedora"

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all esProveedora (plantas linea)

-- 4) Saber si una linea es mixta (no usar length)
lineaMixta :: LineaDeDefensa -> Bool
lineaMixta linea = tieneAlMenosDosPlantas linea && tienePlantasConEspecialidadesDistintas (plantas linea)

tieneAlMenosDosPlantas :: LineaDeDefensa -> Bool
tieneAlMenosDosPlantas linea = not (null (plantas linea)) && not (null (tail (plantas linea)))

tienePlantasConEspecialidadesDistintas :: [Planta] -> Bool
tienePlantasConEspecialidadesDistintas lista
  | null (tail lista) = True
  | especialidadPlanta (head lista) == especialidadPlanta (head (tail lista)) = False
  | otherwise = tienePlantasConEspecialidadesDistintas (tail lista)

-- 5)Resultado de ataques
-- A) Planta -> zombie
ataqueDePlanta :: Planta -> Zombie -> Zombie
ataqueDePlanta planta zombie = zombie {nombre = drop (ataque planta) (nombre zombie)}

-- B) Zombie -> planta
ataqueDeZombie :: Zombie -> Planta -> Planta
ataqueDeZombie zombie planta = planta {vida = vida planta - mordiscos zombie}