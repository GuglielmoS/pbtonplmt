module Main where

import TestFeat
import Test.Feat

main :: IO ()
main = do
  putStrLn "Progress of red"
  featCheck 13 prop_progress
  putStrLn ""

  putStrLn "Preservation of red"
  featCheck 13 prop_preservation
  putStrLn ""

  putStrLn "Confluence of red"
  featCheck 13 prop_confluenceOfRed
  putStrLn ""

  putStrLn "algeq is an equivalence relation"
  putStrLn "reflexivity"
  featCheck 13 prop_isReflAlgeq
  putStrLn "symmetry"
  featCheck 13 prop_isSymAlgeq
  putStrLn "transitivity"
  featCheck 13 prop_isTransAlgeq
  putStrLn ""

  putStrLn "patheq is an equivalence relation"
  --putStrLn "reflexivity"
  --featCheck 13 prop_isReflPatheq
  putStrLn "symmetry"
  featCheck 13 prop_isSymPatheq
  putStrLn "transitivity"
  featCheck 13 prop_isTransPatheq
  putStrLn ""

  --putStrLn "algeq is equivalent to redcomp"
  --featCheck 13 prop_algeqEquivRedcomp
  --putStrLn ""
