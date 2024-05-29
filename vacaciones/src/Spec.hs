module Spec where

import Control.Exception (evaluate)
import Library
import PdePreludat
import Test.Hspec 

correrTests :: IO ()
correrTests = hspec $ do
    describe "parte 1" $ do
        --it "probando excursionesestresantes" $ do
            --excurscionesEstresantes 
        it "probando tour infinito" $ do
            algunTourConvincente ana (tourInfinito ana) `shouldBe` True