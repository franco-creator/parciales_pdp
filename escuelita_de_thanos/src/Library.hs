module Library where

import PdePreludat

type Gema = Personaje -> Personaje

data Guantelete = Guantelete {material :: String, gemas :: [Gema]} deriving (Show, Eq)

data Personaje = Personaje {edad :: Number, energia :: Number, habilidades :: [String], nombre :: String, planeta :: String} deriving (Show, Eq)

type Universo = [Personaje]

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso unGuantelete unUniverso
  | length (gemas unGuantelete) == 6 && material unGuantelete == "uru" = take (div (length unUniverso) 2) (unUniverso)
  | otherwise = unUniverso

-- Parte 2

condicionPendex :: Personaje -> Bool
-- condicionPendex unUniverso = (edad unUniverso) < 45
condicionPendex unUniverso = ((< 45) . edad) unUniverso

universoPendex :: Universo -> Bool
universoPendex unUniverso = any (condicionPendex) unUniverso

masHabilidades :: Personaje -> Bool
-- masHabilidades unUniverso = length(habilidades unUniverso) > 1
masHabilidades unUniverso = ((> 1) . length . habilidades) unUniverso

filtrarPersonajes :: Universo -> Universo
filtrarPersonajes unUniverso = filter masHabilidades unUniverso

sumarEnergia :: Personaje -> Number -> Number
-- sumarEnergia unPersonaje sumatoria = sumatoria + energia unPersonaje
sumarEnergia unPersonaje sumatoria = ((+ sumatoria) . energia) unPersonaje

energiaTotal :: Universo -> Number
energiaTotal unUniverso = foldr (sumarEnergia) 0 (filtrarPersonajes unUniverso)

-- Parte 3

quitarEnergia :: Personaje -> Number -> Number
quitarEnergia unPersonaje cuantoRestar = (energia unPersonaje) - cuantoRestar

-- no me funciono hacer la composicion con la resta de esta manera
-- quitarEnergia unPersonaje cuantoRestar = ((- cuantoRestar) . energia) unPersonaje

mente :: Number -> Personaje -> Personaje
mente unNumero unPersonaje = Personaje (edad unPersonaje) (quitarEnergia unPersonaje unNumero) (habilidades unPersonaje) (nombre unPersonaje) (planeta unPersonaje)

habilidadesDiferentes :: String -> String -> Bool
habilidadesDiferentes unaHabilidad habilidadLista = habilidadLista /= unaHabilidad

-- que pasa si doy vuelta [string] y string?
filtrarHabilidades :: String -> [String] -> [String]
filtrarHabilidades unaHabilidad listaHabilidades = filter (habilidadesDiferentes unaHabilidad) listaHabilidades

alma :: String -> Personaje -> Personaje
alma unaHabilidad unPersonaje = Personaje (edad unPersonaje) (quitarEnergia unPersonaje 10) (filtrarHabilidades unaHabilidad (habilidades unPersonaje)) (nombre unPersonaje) (planeta unPersonaje)

espacio :: String -> Personaje -> Personaje
espacio unPlaneta unPersonaje = Personaje (edad unPersonaje) (quitarEnergia unPersonaje 20) (habilidades unPersonaje) (nombre unPersonaje) unPlaneta

menosHabilidades :: Personaje -> Bool
-- masHabilidades unUniverso = length(habilidades unUniverso) > 1
menosHabilidades unPersonaje = ((<= 2) . length . habilidades) unPersonaje

devolverHabilidades :: Personaje -> [String]
devolverHabilidades unPersonaje
  | menosHabilidades unPersonaje = []
  | otherwise = habilidades unPersonaje

poder :: Personaje -> Personaje
poder unPersonaje = Personaje (edad unPersonaje) 0 (devolverHabilidades unPersonaje) (nombre unPersonaje) (planeta unPersonaje)

dividoEdad :: Number -> Number
dividoEdad unaEdad = unaEdad / 2

edadElTiempo :: Personaje -> Number
edadElTiempo unPersonaje
  | (dividoEdad (edad unPersonaje)) <= 18 = 18
  | otherwise = dividoEdad (edad unPersonaje)

elTiempo :: Personaje -> Personaje
elTiempo unPersonaje = Personaje (edadElTiempo unPersonaje) (quitarEnergia unPersonaje 50) (habilidades unPersonaje) (nombre unPersonaje) (planeta unPersonaje)

{-Forma en que lo hizo el profesor: La nueva edad se calcula dividiendo la edad actual del personaje por 2 y luego tomando el máximo entre ese resultado y 18. Es decir, si la edad del personaje es menor a 18, se tomará el valor 18 como nueva edad.

tiempo :: Gema
tiempo personaje = quitarEnergia 50 personaje {
  edad = (max 18.div (edad personaje)) 2

-}
gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca unaGema unPersonaje = (unaGema . unaGema) unPersonaje

-- Parte 4

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "goma" [elTiempo, alma "usar Mjolnir", gemaLoca (alma "programación en Haskell")]

-- Parte 5

{-lo del efecto de lado: Ahora, hablemos sobre el “efecto de lado”. En programación, el “efecto de lado” se refiere a cualquier cambio que una función realiza fuera de su ámbito local. En este caso, el efecto de lado ocurre cuando aplicamos una gema al personaje. La gema puede modificar alguna propiedad del personaje (como su fuerza, salud, etc.) sin necesariamente devolver un nuevo personaje. Esto significa que la función aplicarGema tiene un efecto de lado sobre el personaje original.
En resumen, la función utilizar ejecuta el poder de cada gema en la lista contra el personaje dado, y el efecto de lado se produce cuando se modifican las propiedades del personaje original debido a la aplicación de las gemas.-}

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema unaGema unPersonaje = unaGema unPersonaje

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas unPersonaje = foldr (aplicarGema) unPersonaje listaGemas

-- Parte 6

{-

asi lo hice yo y no funciona bien

obtenerListaGemas :: Guantelete -> [Gema]
obtenerListaGemas unGuantelete = gemas unGuantelete

compararGema :: [Gema] -> Gema -> Personaje -> Gema
compararGema [] gemaMayor _ = gemaMayor
compararGema (x: xs) gemaMayor unPersonaje
    | energia (x unPersonaje) < energia (x gemaMayor) =  compararGema xs x unPersonaje

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unaPersona = compararGema (obtenerListaGemas unGuantelete head(obtenerListaGemas unGuantelete))-}

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1 : gema2 : gemas)
  | (energia . gema1) personaje < (energia . gema2) personaje = gemaMasPoderosaDe personaje (gema1 : gemas)
  | otherwise = gemaMasPoderosaDe personaje (gema2 : gemas)

-- Parte 7

{-
funcion 1: no se puede ejecutar esa funcion ya que al tener una lista de gemas infinita, eso lo que genera es que no se logre poder mostrar el valor que quiero por pantalla, y eso lo que genera es que no pueda ejecutarse la funcion. Es decir, no logra evaluar todos los elementos y ahi tiene problemas
funcion 2: si se logra ejecutar, ya que la funcion al tomar los primeros tres elementos de la lista infinita eso lo que genera es que se pueda mostrar por pantalla y por lo tanto se puede ejecutar
-}
