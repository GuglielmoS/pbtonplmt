module Main where

import TestFeat
import Test.Feat

main :: IO ()
main = do
  putStrLn "Progress of red"
  featCheck 13 prop_progress
  putStrLn ""

  putStrLn "Preservation of red"
  featCheck 7 prop_preservation
  putStrLn ""
