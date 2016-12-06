{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import STLC
import Properties 
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

import Test.LazySmallCheck

import Control.Exception

instance Serial Ty where
  series = cons0 TyInt \/ cons0 TyList

instance Serial Constant where
  series = cons0 (IntV 0) \/
           cons0 Nil \/
           cons0 Cons \/
           cons0 Hd \/
           cons0 Tl \/
           cons0 Plus

instance Serial (Name Exp) where
  series = cons0 (s2n "a") \/ cons0 (s2n "b")

instance Serial (Bind (Name Exp) Exp) where
  series = cons2 bind

instance Serial Exp where
  series = cons1 Const \/
           cons1 Var \/
           cons2 Lam \/
           cons2 App

progress :: Model -> Exp -> Property
progress stlc e =
  lift (isWellTyped stlc e) *&* lift (not $ isValue stlc e) *&* lift (not $ isError stlc e) *=>* lift (progressCheck stlc e)

preservation :: Model -> Exp -> Property
preservation stlc e = lift (isWellTyped stlc e) *&* lift (canStep stlc e) *=>* lift (preservationCheck stlc e)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

check :: (Exp -> Property) -> IO ()
check prop = catchAny (test prop) $ \_ -> return ()

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
    else case Map.lookup (head args) properties of
           Nothing -> putStrLn "You specified an invalid property!"
           Just property -> do start <- getTime Monotonic
                               check property
                               end <- getTime Monotonic
                               putStr "TIME:"
                               print $ toNanoSecs $ diffTimeSpec start end
                               putStrLn ""
