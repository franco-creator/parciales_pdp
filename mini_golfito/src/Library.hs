module Library where

-- import Library (Alfajor (capasRelleno))

import Foreign.C (CLLong)
import GHC.Num (Num)
import GHC.OldList (isInfixOf)
import GHC.Windows (BOOL)
import PdePreludat


data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--Punto 1

--data Palo = Palo {velocidad :: Number, precision :: Number, altura :: Number}
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro 10 (precisionJugador unaHabilidad * 2) 0 

madera :: Palo
madera unaHabilidad = UnTiro 100 (div (precisionJugador unaHabilidad) 2) 5

dentroDelConjunto :: Number -> Bool
dentroDelConjunto numeroN = between 1 10 numeroN


-- revisarla
hierros :: Number -> Palo
hierros numeroN unaHabilidad = UnTiro (numeroN) (fuerzaJugador unaHabilidad * numeroN) (numeroN - 3)

agregarHierros :: Number -> [Palo]
agregarHierros unNumero | between 1 10 unNumero =  (hierros unNumero) : (agregarHierros (unNumero + 1))

--falta hierros
constantePalos :: [Palo]
constantePalos = [putter, madera] ++ agregarHierros 0


--parte 2
golpe :: Jugador -> Palo -> Tiro
golpe (UnJugador unNombre unPadre unaHabilidad) unPalo = unPalo unaHabilidad


-- parte 3

type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaPrecision > 90 = UnTiro (unaVelocidad * 2) 100 0
  | otherwise = (UnTiro unaVelocidad unaPrecision unaAltura)

laguna :: Number -> Obstaculo
laguna largoLaguna (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaVelocidad > 80 && unaAltura > 1 && unaAltura < 5 = UnTiro unaVelocidad unaPrecision (div unaAltura largoLaguna) 
  | otherwise = (UnTiro unaVelocidad unaPrecision unaAltura)

hoyo :: Obstaculo
hoyo (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaVelocidad > 5 && unaVelocidad < 20 && unaPrecision > 95 = UnTiro  0 0 0
  | otherwise = (UnTiro unaVelocidad unaPrecision unaAltura)

--parte 4

-- necesito esto componerlo con el obstaculo
analizarPalos :: Jugador -> Obstaculo -> Palo -> Bool
analizarPalos unJugador unObstaculo unPalo = (unObstaculo . golpe unJugador) unPalo == golpe unJugador unPalo

--voy a tener que hacer un filter para los palos esos
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (analizarPalos unJugador unObstaculo) constantePalos

--parte 4 b

analizarPasarObstaculo :: Tiro -> Obstaculo -> Bool
analizarPasarObstaculo unTiro unObstaculo = unObstaculo unTiro == unTiro 

{-aplicarObstaculo :: Tiro -> Obstaculo -> Number -> Number
aplicarObstaculo unTiro unObstaculo numeroAcumulado
  | analizarPasarObstaculo unTiro unObstaculo = 
  | analizarPasarObstaculo unTiro unObstaculo = numeroAcumulado + 1
-}

--obstaculosConsectivosSuperados :: [Obstaculo] -> Tiro -> Number
--obstaculosConsectivosSuperados listaObstaculos unTiro = foldr (aplicarObstaculo unTiro) 0 listaObstaculos

obstaculosConsectivosSuperados :: [Obstaculo] -> Tiro -> Number -> Number
obstaculosConsectivosSuperados (x : xs) unTiro contador
  | analizarPasarObstaculo unTiro x = contador
  | otherwise = obstaculosConsectivosSuperados xs unTiro (contador + 1)

--parte 4 c

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador listaObstaculos
  | 










  



