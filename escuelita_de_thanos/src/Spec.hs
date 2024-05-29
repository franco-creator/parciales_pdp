module Spec where

import Control.Exception (evaluate)
import Library
import PdePreludat
import Test.Hspec

guanteleteUno = Guantelete "uru" [mente 5, mente 4, mente 8, mente 1, mente 12, mente 14]

universoParte1 = [Personaje 60 100 ["Super fuerza", "volar", "cantar"] "Iron Man" "Tierra", Personaje 105 90 ["Telepatía", "invisible"] "Jean Grey" "Tierra", Personaje 70 80 ["Regeneración", "caos", "rejuvecimiento"] "Wolverine" "Tierra"]

listaGemas = [mente 5, mente 3, mente 2]

aquaman = Personaje 40 100 ["Super fuerza", "volar"] "Iron Man" "Tierra"

correrTests :: IO ()
correrTests = hspec $ do
  describe "parte 1" $ do
    it "pruebo la funcion charquear" $ do
      chasquearUniverso guanteleteUno universoParte1 `shouldBe` [Personaje 60 100 ["Super fuerza", "volar", "cantar"] "Iron Man" "Tierra"]

  describe "parte 2" $ do
    it "pruebo la funcion pendex" $ do
      universoPendex universoParte1 `shouldBe` False
    it "pruebo la funcion energiaTotal" $ do
      energiaTotal universoParte1 `shouldBe` 270

  describe "parte 3" $ do
    it "Pruebo la funcion mente" $ do
      mente 5 aquaman `shouldBe` Personaje 40 95 ["Super fuerza", "volar"] "Iron Man" "Tierra"
    it "Pruebo la funcion alma" $ do
      alma "volar" aquaman `shouldBe` Personaje 40 90 ["Super fuerza"] "Iron Man" "Tierra"
    it "pruebo la funcion espacio" $ do
      espacio "Marte" aquaman `shouldBe` Personaje 40 80 ["Super fuerza", "volar"] "Iron Man" "Marte"
    it "pruebo la funcion poder" $ do
      poder aquaman `shouldBe` Personaje 40 0 [] "Iron Man" "Tierra"
    it "probando la funcion el tiempo" $ do
      elTiempo aquaman `shouldBe` Personaje 20 50 ["Super fuerza", "volar"] "Iron Man" "Tierra"
    it "pruebo la funcion gema loca" $ do
      gemaLoca (mente 3) aquaman `shouldBe` Personaje 40 94 ["Super fuerza", "volar"] "Iron Man" "Tierra"
  describe "parte 4" $ do
    it "pruebo la funcion utilizar" $ do
      utilizar listaGemas aquaman `shouldBe` Personaje 40 90 ["Super fuerza", "volar"] "Iron Man" "Tierra"
