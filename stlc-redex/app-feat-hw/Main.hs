{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import STLC
import FeatUtil
import Bug1
import Bug2
import Bug3
import Bug4
import Bug5
import Bug6
import Bug7
import Bug8
import Bug9

import Test.Feat
import Test.Feat.Enumerate hiding (Const)

import Unbound.LocallyNameless hiding (Nil)

import System.Clock
import System.Environment
import qualified Data.Map as Map

instance Enumerable Ty where
  enumerate = nullary TyInt <> nullary TyList

instance Enumerable Constant where
  enumerate =  nullary (IntV 0)
            <> nullary Nil
            <> nullary Cons
            <> nullary Hd
            <> nullary Tl
            <> nullary Plus

instance Enumerable Exp where
  enumerate =  pay (unary Const)
            <> pay (unary Var)
            <> pay (unary (funcurry Lam))
            <> pay (unary (funcurry App))

instance Enumerable (Name Exp) where
  enumerate =  nullary (s2n "x")
            <> nullary (s2n "y")

instance Enumerable (Bind (Name Exp) Exp) where
  enumerate = unary (funcurry bind)

main :: IO ()
main = do
  args <- getArgs
  let properties = Map.fromList [("bug1#progress", progress bug1), ("bug1#preservation", preservation bug1),
                                 ("bug2#progress", progress bug2),
                                 ("bug3#progress", progress bug3), ("bug3#preservation", preservation bug3),
                                 ("bug4#progress", progress bug4),
                                 ("bug5#preservation", preservation bug5),
                                 ("bug6#progress", progress bug6),
                                 ("bug7#progress", progress bug7),
                                 ("bug8#preservation", preservation bug8),
                                 ("bug9#preservation", preservation bug9)]
  if length args == 0
    then putStrLn $ "Please, specify the property to test!"
    else let depth = if length args == 2 then read $ args !! 1 else 5
         in case Map.lookup (head args) properties of
              Nothing -> putStrLn "You specified an invalid property!"
              Just property -> do start <- getTime Monotonic
                                  print $ findCounterExample values property depth
                                  end <- getTime Monotonic
                                  putStr "TIME:"
                                  print $ toNanoSecs $ diffTimeSpec start end
                                  putStrLn ""
