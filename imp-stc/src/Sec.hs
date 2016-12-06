{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
  #-}

module Sec where

import IMP

import Data.Map (Map)
import qualified Data.Map as Map

type Level = Int

type SecContext = Map Na Level
type SecTypeChecker = SecContext -> Com -> Level -> Maybe Bool

class Secure a where
  sec :: SecContext -> a -> Maybe Level

instance Secure Na where
  sec ctx x = Map.lookup x ctx 

instance Secure AExp where
  sec _ (N _) = return 0
  sec ctx (Var x) = sec ctx x
  sec ctx (Plus e1 e2) = do
    l1 <- sec ctx e1
    l2 <- sec ctx e2
    return (max l1 l2)

instance Secure BExp where
  sec _ (Bc _) = return 0
  sec ctx (Not b) = sec ctx b
  sec ctx (And b1 b2) = do
    l1 <- sec ctx b1
    l2 <- sec ctx b2
    return (max l1 l2)
  sec ctx (Less b1 b2) = do
    l1 <- sec ctx b1
    l2 <- sec ctx b2
    return (max l1 l2)

eq :: SecContext -> (Level -> Level -> Bool) -> State -> State -> Level -> Bool
eq ctx cmp s t l =
  let s' = Map.filterWithKey (\x _ -> case sec ctx x of
                                        Nothing -> False
                                        Just l' -> l' `cmp` l) s
      t' = Map.filterWithKey (\x _ -> case sec ctx x of
                                        Nothing -> False
                                        Just l' -> l' `cmp` l) t
  in
    and [Map.lookup x s' == Map.lookup x t' | x <- Map.keys s'] &&
    and [Map.lookup x t' == Map.lookup x s' | x <- Map.keys t']

eqLE, eqLT :: SecContext -> State -> State -> Level -> Bool
eqLE ctx = eq ctx (<=)
eqLT ctx = eq ctx (<)
