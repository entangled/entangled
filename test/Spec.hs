module Main where

import Test.Hspec

import ListStreamSpec (listStreamSpec)
import TextUtilSpec (textUtilSpec)

main :: IO ()
main =
    hspec $ listStreamSpec >> textUtilSpec

