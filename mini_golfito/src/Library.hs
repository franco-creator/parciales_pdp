module Library where

-- import Library (Alfajor (capasRelleno))

import Foreign.C (CLLong)
import GHC.Num (Num)
import GHC.OldList (delete, isInfixOf)
import GHC.Windows (BOOL)
import PdePreludat

data Jugador = UnJugador
  { nombre :: String,
    padre :: String,
    habilidad :: Habilidad
  }
  deriving (Eq, Show)

data Habilidad = Habilidad
  { fuerzaJugador :: Number,
    precisionJugador :: Number
  }
  deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro
  { velocidad :: Number,
    precision :: Number,
    altura :: Number
  }
  deriving (Eq, Show)

type Puntos = Number

type JugadorPuntos = (Jugador, Puntos)

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Punto 1

-- data Palo = Palo {velocidad :: Number, precision :: Number, altura :: Number}
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro 10 (precisionJugador unaHabilidad * 2) 0

madera :: Palo
madera unaHabilidad = UnTiro 100 (div (precisionJugador unaHabilidad) 2) 5

dentroDelConjunto :: Number -> Bool
dentroDelConjunto numeroN = between 1 10 numeroN

-- revisarla
hierros :: Number -> Palo
hierros numeroN unaHabilidad = UnTiro numeroN (fuerzaJugador unaHabilidad * numeroN) (numeroN - 3)

agregarHierros :: Number -> [Palo]
agregarHierros unNumero | between 1 10 unNumero = hierros unNumero : agregarHierros (unNumero + 1)

-- falta hierros
constantePalos :: [Palo]
constantePalos = [putter, madera] ++ agregarHierros 0

-- parte 2
golpe :: Jugador -> Palo -> Tiro
golpe (UnJugador unNombre unPadre unaHabilidad) unPalo = unPalo unaHabilidad

-- parte 3 hecha

type Obstaculo = Tiro -> Tiro

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaPrecision > 90 = UnTiro (unaVelocidad * 2) 100 0
  | otherwise = tiroDetenido

laguna :: Number -> Obstaculo
laguna largoLaguna (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaVelocidad > 80 && unaAltura > 1 && unaAltura < 5 = UnTiro unaVelocidad unaPrecision (div unaAltura largoLaguna)
  | otherwise = tiroDetenido

hoyo :: Obstaculo
hoyo (UnTiro unaVelocidad unaPrecision unaAltura)
  | unaVelocidad > 5 && unaVelocidad < 20 && unaPrecision > 95 = UnTiro 0 0 0
  | otherwise = tiroDetenido

-- parte 4

-- necesito esto componerlo con el obstaculo
analizarPalos :: Jugador -> Obstaculo -> Palo -> Bool
analizarPalos unJugador unObstaculo unPalo = (unObstaculo . golpe unJugador) unPalo /= tiroDetenido -- golpe unJugador unPalo

-- voy a tener que hacer un filter para los palos esos
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (analizarPalos unJugador unObstaculo) constantePalos

-- parte 4 b

analizarPasarObstaculo :: Tiro -> Obstaculo -> Bool
analizarPasarObstaculo unTiro unObstaculo = unObstaculo unTiro /= tiroDetenido

{-aplicarObstaculo :: Tiro -> Obstaculo -> Number -> Number
aplicarObstaculo unTiro unObstaculo numeroAcumulado
  | analizarPasarObstaculo unTiro unObstaculo =
  | analizarPasarObstaculo unTiro unObstaculo = numeroAcumulado + 1
-}

-- obstaculosConsectivosSuperados :: [Obstaculo] -> Tiro -> Number
-- obstaculosConsectivosSuperados listaObstaculos unTiro = foldr (aplicarObstaculo unTiro) 0 listaObstaculos

obstaculosConsectivosSuperados :: [Obstaculo] -> Tiro -> Number
obstaculosConsectivosSuperados [] _ = 0
obstaculosConsectivosSuperados (x : xs) unTiro | (analizarPasarObstaculo unTiro) x = 1 + obstaculosConsectivosSuperados xs unTiro

-- parte 4 c

analizarPaloMasUtil :: Jugador -> Obstaculo -> [Palo] -> [Palo]
analizarPaloMasUtil unJugador unObstaculo listaPalos = filter (analizarPalos unJugador unObstaculo) listaPalos

-- podes usar un filter con un foldr y de esa manera se van perdiendo los que
paloMasUtil :: Jugador -> [Obstaculo] -> [Palo]
paloMasUtil unJugador listaObstaculos = foldr (analizarPaloMasUtil unJugador) constantePalos listaObstaculos

-- analizarJugadores :: JugadorPuntos -> [String] -> [String]
-- analizarJugadores jugadoresPuntos listaPadres =

-- hacer un foldr para encontrar el que tiene maximo puntaje de los jugadores. A ese sacarlo, y luego hacer un map y devolver una lista de los demas padres
-- puntoPadres :: [JugadorPuntos] -> [String]
-- puntoPadres listaJugadores = foldr (analizarJugadores) [] listaJugadores

buscarJugadorGanador :: [JugadorPuntos] -> JugadorPuntos
buscarJugadorGanador [x] = x
buscarJugadorGanador ((jugadorUno, puntajeUno) : (jugadorDos, puntajeDos) : xs)
  | puntajeUno < puntajeDos = buscarJugadorGanador ((jugadorDos, puntajeDos) : xs)
  | otherwise = buscarJugadorGanador ((jugadorUno, puntajeDos) : xs)

eliminarPadreGanador :: JugadorPuntos -> [JugadorPuntos] -> [JugadorPuntos]
eliminarPadreGanador jugadorGanador listaJugadores = delete jugadorGanador listaJugadores

sacoPadres :: JugadorPuntos -> String
sacoPadres (unJugador, unosPuntos) = padre unJugador

sacarPadres :: [JugadorPuntos] -> [String]
sacarPadres listaJugadoresSinGanador = map (sacoPadres) listaJugadoresSinGanador

puntoPadres :: [JugadorPuntos] -> [String]
puntoPadres listaJugadores = (sacarPadres . flip eliminarPadreGanador listaJugadores . buscarJugadorGanador) listaJugadores
