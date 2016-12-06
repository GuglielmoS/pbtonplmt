{- fullsimple implemenation.  

 Example Usage: "./f -I../ test.f" will try to evaluate the terms in
 the file "test.f", with "../" added to the search path.
 -}

module Main where

import Syntax
import Evaluator
import Typing
import SimpleContext
import TaplError

import TestFeat
import Properties

import Test.Feat

main :: IO ()
main = do
  putStrLn "Progress"
  featCheck 10 progressCheck
  putStrLn ""
  putStrLn "Preservation"
  featCheck 10 preservationCheck
