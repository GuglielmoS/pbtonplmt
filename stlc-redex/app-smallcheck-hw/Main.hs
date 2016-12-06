{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import STLC
import SmallCheckUtil
import Bug1
import Bug2
import Bug3
import Bug4
import Bug5
import Bug6
import Bug7
import Bug8
import Bug9

import Unbound.LocallyNameless hiding (Nil)

import System.Clock
import System.Environment
import qualified Data.Map as Map

import Test.SmallCheck
import Test.SmallCheck.Series

instance Monad m => Serial m Constant where
  series = cons0 (IntV 0) \/
           cons0 Nil \/
           cons0 Cons \/
           cons0 Hd \/
           cons0 Tl \/
           cons0 Plus

instance Monad m => Serial m Exp where
  series = newtypeCons Const \/
           newtypeCons Var \/
           newtypeCons (Lam TyInt) \/
           newtypeCons (Lam TyList) \/
           cons2 App

instance Monad m => Serial m (Name Exp) where
  series = cons0 (s2n "a") \/ cons0 (s2n "b")

instance Monad m => Serial m (Bind (Name Exp) Exp) where
  series = cons2 bind

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
                                  smallCheck depth property
                                  end <- getTime Monotonic
                                  putStr "TIME:"
                                  print $ toNanoSecs $ diffTimeSpec start end
                                  putStrLn ""

