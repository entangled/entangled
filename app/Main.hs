module Main where

import Parser
import Model

main :: IO ()
main = do
    source <- readFile "test/test01.md"
    let doc = parseMarkdown source
    print doc
