module Library where

import Data.Bool (bool)
import GHC.Base (Alternative, TrName)
import GHC.IO.Encoding.CodePage (localeEncoding)
import PdePreludat

type Idioma = String

data Turista = Turista
  { cansancio :: Number,
    stress :: Number,
    solitario :: Bool,
    idiomas :: [Idioma]
  }
  deriving (Eq, Show)

cathi :: Turista
cathi = Turista 15 15 True ["mali", "catalan"]

type Excursion = Turista -> Turista

type Indice = Turista -> Number

aumentarODisminuirCansancio :: Number -> Turista -> Turista
aumentarODisminuirCansancio numero unTurista = unTurista {cansancio = cansancio unTurista + numero}

aumentarODisminuirStress :: Number -> Turista -> Turista
aumentarODisminuirStress numero unTurista = unTurista {stress = stress unTurista + numero}

irPlaya :: Turista -> Turista
irPlaya unTurista
  | solitario unTurista = aumentarODisminuirCansancio (-5) unTurista
  | otherwise = aumentarODisminuirStress (-1) unTurista

apreciarElementoPaisaje :: String -> Turista -> Turista
apreciarElementoPaisaje elemento unTurista = aumentarODisminuirStress (length elemento) unTurista

intensidad :: Number -> Number
intensidad unaIntensidad = div unaIntensidad 4

caminar :: Number -> Excursion
caminar caminado unTurista = aumentarODisminuirCansancio (intensidad caminado) unTurista

data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)

paseoBarco :: Marea -> Turista -> Turista
paseoBarco Tranquila unTurista = (caminar 10 . apreciarElementoPaisaje "mar" . hablarIdioma "Aleman") unTurista

hablarIdioma :: Idioma -> Turista -> Turista
hablarIdioma idioma unTurista
  | elem idioma (idiomas unTurista) = unTurista
  | otherwise = unTurista {idiomas = idioma : idiomas unTurista}

reduccionStress :: Turista -> Turista
reduccionStress (Turista unCansancio unStress unaSoledad unosIdiomas) = Turista unCansancio (unStress - (div unStress 10)) unaSoledad unosIdiomas

funcionExcursion :: Turista -> Excursion -> Turista
funcionExcursion unTurista unaExcursion = (reduccionStress . unaExcursion) unTurista

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = reduccionStress . excursion

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun f turista excursion = deltaSegun f (hacerExcursion excursion turista) turista

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista

esDescestresante :: Turista -> Excursion -> Bool
esDescestresante unTurista unaExcursion = ((<= 3) . deltaExcursionSegun stress unTurista) unaExcursion

excursionesDescestresantes :: Turista -> Tour -> Tour
excursionesDescestresantes unTurista unTour = filter (esDescestresante unTurista) unTour

-- (<= 3) . deltaExcursionSegun stress unTurista

-- parte 3

type Tour = [Excursion]

tourCompleto :: Turista -> Tour
tourCompleto unTurista = [caminar 20, apreciarElementoPaisaje "cascada", caminar 40, hablarIdioma "melmacquiano"]

tourLadoB :: Excursion -> Tour
tourLadoB unaExcursion = [paseoBarco Tranquila, unaExcursion, caminar 120]

agregarExcursionesIslaVecina :: Marea -> Excursion
agregarExcursionesIslaVecina unaMarea
  | unaMarea == Fuerte = apreciarElementoPaisaje "lago"
  | otherwise = irPlaya

tourIslaVecina :: Marea -> Tour
tourIslaVecina unaMarea = [paseoBarco unaMarea, agregarExcursionesIslaVecina unaMarea, paseoBarco unaMarea]

-- como hago para aplicar x veces una funcion?
hacerTour :: Tour -> Turista -> Turista
hacerTour unTour unTurista = foldr (hacerExcursion) unTurista unTour

-- hacerTour :: Turista -> Tour -> Turista
-- hacerTour turista tour =
-- foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

analizarSoledad :: Bool -> Turista -> Bool
analizarSoledad unBool unTurista = unBool == solitario unTurista

analizarExcursiones :: Turista -> Excursion -> Bool
analizarExcursiones unTurista unaExcursion = (not . solitario . hacerExcursion unaExcursion) unTurista

analizarTours :: Turista -> Tour -> Bool
analizarTours unTurista unTour = (any (analizarExcursiones unTurista) . excursionesDescestresantes unTurista) unTour

tourConvincente :: [Tour] -> Turista -> Bool
tourConvincente listaTours unTurista = any (analizarTours unTurista) listaTours

filtrarConvincente :: Tour -> [Turista] -> [Turista]
filtrarConvincente unTour listaTuristas = filter (flip analizarTours unTour) listaTuristas 


-- filtro los que son convincentes y luego aplico lo de la espiritualidad y le hago la sumatoria para acumularlo
efectividadTour :: Tour -> [Turista] -> Number
efectividadTour unTour listaTuristas = espiritualidad . filtrarConvincente unTour listaTuristas