{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
import Data.ByteString (elemIndex)

-- Declarar los data que se van a usar
data Planta = Planta
  { vida :: Int,
    soles :: Int,
    ataque :: Int
  }
  deriving (Show, Eq)

data Zombie = Zombie
  { nombre :: String,
    accesorios :: [String],
    mordiscos :: Int
  }
  deriving (Show, Eq)

data LineaDeDefensa = LineaDeDefensa
  { plantas :: [Planta],
    zombies :: [Zombie]
  }
  deriving (Show, Eq)

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

linea5 =
  LineaDeDefensa
    { plantas = [sunFlower, peaShooter, superPeaShooter, superSunFlower, nut, superPeaShooter],
      zombies = zombieBase : zombies linea5
    }

linea6 =
  LineaDeDefensa
    { plantas = peaShooter : plantas linea6,
      zombies = [gargantuar, zombieBase, zombieBase]
    }

linea7 =
  LineaDeDefensa
    { plantas = sunFlower : plantas linea7,
      zombies = [gargantuar, zombieBase, zombieBase]
    }

linea8 =
  LineaDeDefensa
    { plantas = [peaShooter, peaShooter, peaShooter, peaShooter, peaShooter, peaShooter],
      zombies = []
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
zombieBase = Zombie "ZombieBase" [] 1

balloonZombie = Zombie "BalloonZombie" ["Globo"] 1

newspaperZombie = Zombie "NewspaperZombie" ["Diario"] 2

gargantuar = Zombie "GargantuarHulkSmashPunyGod" ["Poste Electrico", "Zombie Enano"] 30

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte = length . nombre

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
totalAtaquePlantas = sum . map ataque . plantas

totalMordiscosZombies :: LineaDeDefensa -> Int
totalMordiscosZombies = sum . map mordiscos . zombies

estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (totalAtaquePlantas linea < totalMordiscosZombies linea) || (all esPeligroso (zombies linea) && not (null (zombies linea)))

-- C)Saber si una linea necesita ser defendida
esProveedora :: Planta -> Bool
esProveedora planta = especialidadPlanta planta == "Proveedora"

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida = all esProveedora . plantas

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
ataqueDePlanta planta zombie = zombie {nombre = drop (ataque planta) (nombre zombie), accesorios = quitarGlobo (accesorios zombie)}

quitarGlobo :: [String] -> [String]
quitarGlobo = filter (/= "Globo")

-- B) Zombie -> planta
ataqueDeZombie :: Zombie -> Planta -> Planta
ataqueDeZombie zombie planta = planta {vida = if vida planta > mordiscos zombie then vida planta - mordiscos zombie else 0}

-- PARTE 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
1)
i)Si hubiera una cantidad infinita de zombies base en una linea, al evaluar si una linea esta en peligro,
la funcion nunca dejaria de ejecutar ya que usa funciones como map o sum que iteran para todos los elementos de la lista.

ii)Si hubiera una cantidad infinita de PeaShooters en una linea, al evaluar si necesita ser defendida,
retorna false ya que haskell utiliza lasy evaluation, lo que significa que iterara por la lista hasta encontrr
una planta que no sea proveedora, cuando la encuentre dejara de iterar (por mas que queden mas elementos en la lista) y retornara false.

Si hubiera una cantidad infinita de sunFlowers, la funcion nunca dejaria de ejecutar
ya que seguira iterando indefinidamente en busca de una planta no proveedora nunca la encontrara ya que solo hay sunFlowers.
-}
-- 2)
cactus = Planta 9 0 0

-- 3)
type Horda = [(Zombie, LineaDeDefensa)]

hordaUno :: Horda
hordaUno = [(zombieBase, linea1), (zombieBase, linea2), (zombieBase, linea3)]

hordaBase :: Horda
hordaBase = [(zombieBase, linea1), (zombieBase, linea2), (zombieBase, linea3), (zombieBase, linea1), (zombieBase, linea2), (zombieBase, linea3)]

septimoRegimiento :: Horda
septimoRegimiento = [(newspaperZombie, linea2), (balloonZombie, linea1), (balloonZombie, linea1), (balloonZombie, linea3), (balloonZombie, linea3)]

region :: Horda
region = [(gargantuar, linea1), (gargantuar, linea2), (gargantuar, linea3), (gargantuar, linea1), (gargantuar, linea2), (gargantuar, linea3)]

type Jardin = [LineaDeDefensa]

jardin1 :: Jardin
jardin1 = [linea1, linea2, linea3]

agregarHordaAJardin :: Jardin -> Horda -> Jardin
agregarHordaAJardin jardin horda = [agregarHordaALinea linea horda | linea <- jardin]

agregarHordaALinea :: LineaDeDefensa -> Horda -> LineaDeDefensa
agregarHordaALinea linea horda = linea {zombies = zombies linea ++ map fst (filter ((== linea) . snd) horda)}

-- 4)
rondaDeAtaque :: Planta -> Zombie -> Int -> (Planta, Zombie)
rondaDeAtaque planta zombie cantAtaques = (nAtaquesDeZombie planta zombie cantAtaques, ataqueDePlanta planta zombie)

nAtaquesDeZombie :: Planta -> Zombie -> Int -> Planta
nAtaquesDeZombie planta _ 0 = planta
nAtaquesDeZombie planta zombie cantAtaques = nAtaquesDeZombie (ataqueDeZombie zombie planta) zombie (cantAtaques - 1)

-- 5)
plantaEstaMuerta :: Planta -> Bool
plantaEstaMuerta = (<= 0) . vida

zombieEstaMuerto :: Zombie -> Bool
zombieEstaMuerto = null . nombre

-- 6)
ataqueSistematico :: [Planta] -> Zombie -> [Planta]
ataqueSistematico plantas zombie = [ataqueDeZombie zombie planta | planta <- plantas]

-- 7)
resultadoDeAtaque :: LineaDeDefensa -> Horda -> LineaDeDefensa
resultadoDeAtaque linea horda = ataqueAMuerte (linea {zombies = filter (not . esPeligroso) (zombies (agregarHordaALinea linea horda))})

ataqueAMuerte :: LineaDeDefensa -> LineaDeDefensa
ataqueAMuerte linea
  | (null (plantas linea) && not (zombieEstaMuerto (head (zombies linea)))) || (null (zombies linea) && not (plantaEstaMuerta (last (plantas linea)))) = linea
  | zombieEstaMuerto (head (zombies linea)) = ataqueAMuerte (linea {zombies = tail (zombies linea)})
  | plantaEstaMuerta (last (plantas linea)) = ataqueAMuerte (linea {plantas = init (plantas linea)})
  | otherwise = ataqueAMuerte (ataqueMutuo linea)

ataqueMutuo :: LineaDeDefensa -> LineaDeDefensa
ataqueMutuo linea = linea {plantas = init (plantas linea) ++ [ataqueDeZombie (head (zombies linea)) (last (plantas linea))], zombies = ataqueDePlanta (last (plantas linea)) (head (zombies linea)) : tail (zombies linea)}

-- 8)
theZombiesAteYourBrains :: Jardin -> Horda -> Bool
theZombiesAteYourBrains jardin horda = gananLosZombies (agregarHordaAJardin jardin horda)

gananLosZombies :: Jardin -> Bool
gananLosZombies = all muerenTodasLasPlantas

muerenTodasLasPlantas :: LineaDeDefensa -> Bool
muerenTodasLasPlantas linea = not (any ((> 0) . vida) (ataqueEnSerie linea))

ataqueEnSerie :: LineaDeDefensa -> [Planta]
ataqueEnSerie linea
  | null (zombies linea) = plantas linea
  | otherwise = ataqueEnSerie linea {plantas = ataqueSistematico (plantas linea) (head (zombies linea)), zombies = tail (zombies linea)}

-- 9)

tieneMenosLetras :: Zombie -> LineaDeDefensa -> Bool
tieneMenosLetras zombie linea = (\z l -> nivelDeMuerte z > foldl (\acc x -> acc + nivelDeMuerte x) 0 (zombies l)) zombie linea

totalLetrasLinea :: LineaDeDefensa -> Int
totalLetrasLinea linea
  | null (zombies linea) = 0
  | otherwise = (\lin -> nivelDeMuerte (head (zombies lin)) + totalLetrasLinea (lin {zombies = tail (zombies lin)})) linea

{-
10)
La funcion recibe un elemento que debe ser comparable, una funcion, una tupla y unn lista.
La funcion usa guardas para chequear si H pertenese a una lista, si pertenece,
filtra todos los elementos de la lista comparandolos con H a travez de la funcion M.
Luego retorna el primero de la lista, si H no pertenece a la lista retorna el primer elemento de la tupla

A)Conceptos:
• Guardas
• Pattern Matching
• Funciones de orden superior
• Composición
• Tuplas
-}
-- B)

f :: Eq a => a -> (a -> a -> Bool) -> (a, b) -> [a] -> a
f h m (p, _) lista
  | h `elem` lista = head (filter (m h) lista)
  | otherwise = p

{-
C) si la lista fuera infinita, debido a que haskell utiliza lazy evaluation, pueden existir dos casos:
Si H es un elemento alcanzable de la lista, entonces realizara el filter y devolvera el primero de
la lista resultante por mas que el filter no sea realizado para todos los elementos de la lista.
Si H no es un elemento de la lista (o esta ultimo luego de infinitos elementos), la funcion nunca finalizaria de ejecutar.
-}

nivelSupervivencia :: LineaDeDefensa -> Int
nivelSupervivencia = \l -> sum (map vida (plantas l)) - foldl (\acc z -> acc + nivelDeMuerte z) 0 (zombies l)
