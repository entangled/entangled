module Main where

import Test.Hspec

import Config (readLocalConfig)
import ListStreamSpec (listStreamSpec)
import TangleSpec (tangleSpec, tangleEqualSpec)
import TextUtilSpec (textUtilSpec)

main :: IO ()
main = do
    cfg <- readLocalConfig
    hspec $ listStreamSpec >> tangleSpec cfg >> tangleEqualSpec cfg >> textUtilSpec

