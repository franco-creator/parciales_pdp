module Library where

import Data.Bool (bool)
import GHC.Base (TrName)
import GHC.IO.Encoding.CodePage (localeEncoding)
import PdePreludat

data Turista = Turista {cansancio :: Number, stress :: Number, soledad :: Bool, idiomas :: [String]} deriving (Show, Eq)

type Excursion = Turista -> Turista

type Indice = Turista -> Number

aumentarCansancioOStress :: Number -> Number -> Number
aumentarCansancioOStress unValor disminucion = unValor - disminucion

disminuirCansancioOStress :: Number -> Number -> Number
disminuirCansancioOStress unValor aumento = unValor + aumento

irPlaya :: Turista -> Turista
irPlaya (Turista unCansancio unStress unaSoledad unosIdiomas)
  | unaSoledad == True = Turista (aumentarCansancioOStress unCansancio 5) unStress unaSoledad unosIdiomas
  | otherwise = Turista unCansancio (unStress - 1) unaSoledad unosIdiomas

apreciarElementoPaisaje :: String -> Turista -> Turista
apreciarElementoPaisaje apreciado unTurista = Turista (cansancio unTurista) (aumentarCansancioOStress (stress unTurista) (length apreciado)) (soledad unTurista) (idiomas unTurista)

intensidad :: Number -> Number
intensidad unaIntensidad = div unaIntensidad 4

caminar :: Number -> Turista -> Turista
caminar caminado (Turista unCansancio unStress unaSoledad unosIdiomas) = Turista (aumentarCansancioOStress unCansancio (intensidad caminado)) (disminuirCansancioOStress unStress (intensidad caminado)) unaSoledad unosIdiomas

hablarIdioma :: String -> Turista -> Turista
hablarIdioma idiomaAprendido (Turista unCansancio unStress unaSoledad unosIdiomas)
  | (elem idiomaAprendido unosIdiomas) == False = Turista unCansancio unStress False (idiomaAprendido : unosIdiomas)
  | otherwise = Turista unCansancio unStress False unosIdiomas

-- es necesario agregar una funcion en el otherwise que agregue a la lista de idiomas el idioma aleman si es que no esta en la lista?

data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)

paseoBarco :: Marea -> Turista -> Turista
paseoBarco marea unTurista
  | marea == Fuerte = Turista (disminuirCansancioOStress (cansancio unTurista) 10) (disminuirCansancioOStress (stress unTurista) 6) (soledad unTurista) (idiomas unTurista)
  | marea == Moderada = Turista (cansancio unTurista) (stress unTurista) (soledad unTurista) (idiomas unTurista)
  | otherwise = ((apreciarElementoPaisaje "mar") . (caminar 10)) unTurista


cathi :: Turista
cathi = Turista 15 15 True ["Aleman", "catalan"]

reduccionStress :: Turista -> Turista
reduccionStress (Turista unCansancio unStress unaSoledad unosIdiomas) = Turista unCansancio (unStress - (div unStress 10)) unaSoledad unosIdiomas

funcionExcursion :: Turista -> Excursion -> Turista
funcionExcursion unTurista unaExcursion = (reduccionStress . unaExcursion) unTurista

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Number
deltaExcursionSegun unIndice unTurista unaExcursion = (unIndice (unaExcursion unTurista)) - (unIndice unTurista)

-- indice = Turista -> number
-- Excursion = turista -> turista
-- turista =

longitudListaIdiomas :: Turista -> Number
longitudListaIdiomas unTurista = length (idiomas unTurista)

excursionEducativa :: String -> Turista -> Excursion -> Bool
excursionEducativa idioma unTurista unaExcursion
  | deltaExcursionSegun (longitudListaIdiomas) unTurista (hablarIdioma idioma) == -1 = True
  | otherwise = False

devolverStress :: Turista -> Number
devolverStress unTurista = stress unTurista

-- asi la pense en un principio
-- excursionesDescestresantes :: Turista ->  Excursion -> [String]
-- excursionesDescestresantes unTurista unaExcursion = filter (deltaExcursionSegun devolverStress unTurista unaExcursion) (idiomas unTurista)

-- por que se tiene que sacar afuera la excursion como lo que le estas pasando a la composicion?
excursionesDescestresantes :: Turista -> Excursion -> Bool
excursionesDescestresantes unTurista unaExcursion = ((<= -3 ) . (deltaExcursionSegun devolverStress unTurista)) (unaExcursion)

-- Punto 3

type Tour = [Excursion]

completo :: Tour
completo = [apreciarElementoPaisaje "cascada", irPlaya, hablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursionElegida = [paseoBarco Tranquila, excursionElegida, caminar 2]

-- ver si debe de estar ordenado la lista de excursiones

agregarExcursion :: Marea -> [Excursion] -> Tour
agregarExcursion unaMarea listaExcursiones
  | unaMarea == Fuerte = (apreciarElementoPaisaje "laguna" : listaExcursiones)

islaVecina :: Marea -> Tour
islaVecina unaMarea = agregarExcursion unaMarea [paseoBarco Tranquila, paseoBarco Tranquila]

aplicarExcursion :: Excursion -> Turista -> Turista
aplicarExcursion unaExcursion unTurista = unaExcursion unTurista

aplicarExcursiones :: Tour -> Turista -> Turista
aplicarExcursiones unTour unTurista = foldr (aplicarExcursion) unTurista unTour

cantidadExcursiones :: Tour -> Turista -> Turista
cantidadExcursiones unTour (Turista unCansancio unStress unaSoledad unosIdiomas) = Turista unCansancio (unStress - (length unTour)) unaSoledad unosIdiomas

-- anotar que es el orden lo que hace que se saque afuera de la composicion
-- anotar, puede ser que sea util utilizar composicion cuando en una funcion quiero aplicar 2 o mas funciones?

hacerTour :: Tour -> Turista -> Turista
-- hacerTour unTour unTurista = ((aplicarExcursiones .length) unTour
hacerTour unTour unTurista = ((cantidadExcursiones unTour) . (aplicarExcursiones unTour)) unTurista

analizarSoledad :: Bool -> Turista -> Bool
analizarSoledad boolFuncionAnterior unTurista = (soledad unTurista) == False && boolFuncionAnterior == True

-- no se pueden componer todas las funciones entonces?
analizarConvincente :: Turista -> Excursion -> Bool
analizarConvincente unTurista unaExcursion = analizarSoledad (excursionesDescestresantes unTurista unaExcursion) unTurista -- esto esta mal && unaExcursion unTurista == hablarIdioma "melmacquiano" unTurista
-- analizarConvincente unTurista unaExcursion = ((analizarSoledad unTurista) . excursionesDescestresantes unTurista) unaExcursion

algunTourConvincente :: Turista -> [Excursion] -> Bool
algunTourConvincente unTurista listaTours = any (analizarConvincente unTurista) listaTours

-- pongo un map ya que le quiero aplicar la excursion a todos los turistas
-- hacerla despues si podes esta funcion por que es bastante mas complicada que lo normal
{-
perdidaStressYCansancio :: [Turista] -> Excursion -> Number -> Number
perdidaStressYCansancio (x: xs) unaExcursion unNumero = map (deltaExcursionSegun (devolverStress unTurista) unTurista unaExcursion) unTurista

espiritualidadTurista :: Tour -> [Turista] -> Number
espiritualidadTurista unTour unTurista = foldr (perdidaStressYCansancio unTurista) 0 unTour

efectividadTour :: Tour -> [Turista] -> Number
efectividadTour unTour conjuntoTuristas = {-sumatoriaEspiritualidad .-} espiritualidadTurista unTour conjuntoTuristas

-}



tourInfinito :: Turista -> Tour
tourInfinito unTurista = (irPlaya) : (tourInfinito unTurista)

-- las dos son falsas

ana :: Turista
ana = Turista 0 21 False ["Español"]

beto :: Turista
beto = Turista 15 15 True ["Aleman"]