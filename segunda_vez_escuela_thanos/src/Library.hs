module Library where

import PdePreludat

type Gema = Personaje -> Personaje

data Guantelete = Guantelete {material :: String, gemas :: [Gema]} deriving (Show, Eq)

data Personaje = Personaje {edad :: Number, energia :: Number, habilidades :: [String], nombre :: String, planeta :: String} deriving (Show, Eq)

type Universo = [Personaje]

reducirMitadPersonajes :: Universo -> Universo
reducirMitadPersonajes unUniverso = take (div (length unUniverso) 2) unUniverso

-- no puedo hacer aca pattern matching por que tengo que contar la cantidad de elementos de la lista
chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso unGuantelete unUniverso
  | length (gemas unGuantelete) == 6 && material unGuantelete == "uru" = reducirMitadPersonajes unUniverso
  | otherwise = unUniverso

-- funciona pero la hago mas eficiente
{-chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso unGuantelete unUniverso
  | length (gemas unGuantelete) == 6 && material unGuantelete == "uru" = take (div (length unUniverso) 2) (unUniverso)
  | otherwise = unUniverso
-}

-- Parte 2

condicionPendex :: Personaje -> Bool
-- condicionPendex unUniverso = (edad unUniverso) < 45
condicionPendex unUniverso = ((< 45) . edad) unUniverso

universoPendex :: Universo -> Bool
universoPendex unUniverso = any (condicionPendex) unUniverso

masHabilidades :: Personaje -> Bool
-- masHabilidades unUniverso = length(habilidades unUniverso) > 1
masHabilidades unUniverso = ((> 1) . length . habilidades) unUniverso

-- era de como hice antes
-- sumarEnergia :: Personaje -> Number -> Number
-- sumarEnergia unPersonaje sumatoria = sumatoria + energia unPersonaje
-- sumarEnergia unPersonaje sumatoria = ((+ sumatoria) . energia) unPersonaje

obtenerSoloEnergia :: Personaje -> Number
obtenerSoloEnergia unPersonaje = energia unPersonaje

-- necesito hacer primero algo que me filtre los que tienen mas de una habilidad, luego sacarles su energia y luego hacer un foldr para hacer la sumatoria y devolverlo
energiaTotal :: Universo -> Number
energiaTotal unUniverso = (sum . map (obtenerSoloEnergia) . filter (masHabilidades)) unUniverso

-- energiaTotal :: Universo -> Number
-- energiaTotal unUniverso = foldr (sumarEnergia) 0 (filtrarPersonajes unUniverso)

-- Parte 3

-- era de como lo hice antes
quitarEnergia :: Personaje -> Number -> Number
quitarEnergia unPersonaje cuantoRestar = (energia unPersonaje) - cuantoRestar

-- no me funciono hacer la composicion con la resta de esta manera con la de arriba funcion
-- quitarEnergia unPersonaje cuantoRestar = ((- cuantoRestar) . energia) unPersonaje

aumentarODisminuirEnergia :: Number -> Personaje -> Personaje
aumentarODisminuirEnergia cuantoDebilitar unPersonaje = unPersonaje {energia = energia unPersonaje + cuantoDebilitar}

mente :: Number -> Personaje -> Personaje
mente cuantoDebilitar unPersonaje = aumentarODisminuirEnergia (-cuantoDebilitar) unPersonaje

-- funcionaba pero la mejoro
-- mente :: Number -> Personaje -> Personaje
-- mente unNumero unPersonaje = Personaje (edad unPersonaje) (quitarEnergia unPersonaje unNumero) (habilidades unPersonaje) (nombre unPersonaje) (planeta unPersonaje)

analizarEliminar :: String -> String -> Bool
analizarEliminar habilidadBuscada habilidadObtenidaLista = habilidadBuscada /= habilidadObtenidaLista

-- buscar la habilidad con un foldr, teniendo una funcion que me permita devolver la habilidad si no es esa
analizarEliminarHabilidad :: String -> Personaje -> Personaje
analizarEliminarHabilidad habilidadBuscada unPersonaje = unPersonaje { habilidades = (filter (analizarEliminar habilidadBuscada) . habilidades) unPersonaje } 

alma :: String -> Personaje -> Personaje
alma habilidadBuscada unPersonaje = (analizarEliminarHabilidad habilidadBuscada . aumentarODisminuirEnergia (-10)) unPersonaje


cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta planetaCambiado unPersonaje = unPersonaje {planeta = planetaCambiado}

espacio :: String -> Personaje -> Personaje
espacio planetaCambiado unPersonaje = (aumentarODisminuirEnergia (-20) . cambiarPlaneta planetaCambiado) unPersonaje

eliminarEnergia :: Personaje -> Personaje
eliminarEnergia unPersonaje = unPersonaje {energia = 0}

quitarHabilidades :: Personaje -> Personaje 
quitarHabilidades unPersonaje = unPersonaje {habilidades = []}

eliminarCapazHabilidades :: Personaje -> Personaje
eliminarCapazHabilidades unPersonaje 
  | ((<= 2) . length . habilidades) unPersonaje = quitarHabilidades unPersonaje
  | otherwise = unPersonaje

poder :: Personaje -> Personaje 
poder unPersonaje = (eliminarEnergia . eliminarCapazHabilidades) unPersonaje

analizarDisminuirEdad :: Personaje -> Personaje
analizarDisminuirEdad unPersonaje 
  | ((<= 18) . div (edad unPersonaje)) 2 = unPersonaje {edad = 18}
  | otherwise = unPersonaje { edad = div (edad unPersonaje) 2} 

tiempo :: Personaje -> Personaje
tiempo unPersonaje = (aumentarODisminuirEnergia (-50) . analizarDisminuirEdad ) unPersonaje

gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca unaGema unPersonaje = (unaGema . unaGema) unPersonaje

--parte 4
guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "goma" [tiempo,alma "usar Mjolnir", gemaLoca (alma "programación en Haskell")]

--punto 5

aplicarGemas :: Gema -> Personaje -> Personaje
aplicarGemas unaGema unPersonaje = unaGema unPersonaje

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas unPersonaje = foldr (aplicarGemas) unPersonaje listaGemas