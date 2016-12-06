{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , DeriveGeneric
  #-}

module IMP where

import Data.Map as M

import Unbound.LocallyNameless hiding (Nil)

import qualified GHC.Generics as GHC

type Na = Name AExp

data AExp = N Int
          | Var Na
          | Plus AExp AExp
          deriving (Show, GHC.Generic)

$(derive [''AExp])

instance Alpha AExp
instance Subst AExp AExp where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

type State = Map Na Int

aeval :: AExp -> State -> Maybe Int
aeval (N n) _ = return n
aeval (Var x) st = M.lookup x st
aeval (Plus a1 a2) s = do
  a1' <- aeval a1 s
  a2' <- aeval a2 s
  return (a1' + a2')

data BExp = Bc Bool
          | Not BExp
          | And BExp BExp
          | Less AExp AExp
          deriving (Show, GHC.Generic)

$(derive [''BExp])

instance Alpha BExp

beval :: BExp -> State -> Maybe Bool
beval (Bc v) _ = return v
beval (Not b) s = do
  b' <- beval b s
  return (not b')
beval (And b1 b2) s = do
  b1' <- beval b1 s
  b2' <- beval b2 s
  return (b1' && b2')
beval (Less a1 a2) s = do
  a1' <- aeval a1 s
  a2' <- aeval a2 s
  return (a1' < a2')

data Com = Skip
         | Assign Na AExp
         | Seq    Com  Com         
         | If     BExp Com Com     
         | While  BExp Com
         deriving (Show, GHC.Generic)

$(derive [''Com])

instance Alpha Com

ceval :: Com -> State -> Maybe State
ceval Skip st = return  st
ceval (Assign x a)  st = do
  a' <- aeval a st
  return (insert x a' st)
ceval (Seq c1 c2) st = do
  st' <- ceval c1 st
  ceval c2 st'
ceval (If b c1 c2) st = do
  cond <- beval b st
  if cond then
    ceval c1 st
  else
    ceval c2 st
ceval (While b c) st = do
  cond <- beval b st
  if cond then do
    st' <- ceval c st
    ceval (While b c) st'
  else
    return st

cevalN :: Int -> Com -> State -> Maybe State
cevalN 0 _ _ = Nothing
cevalN _ Skip st = return st
cevalN _ (Assign x a) st = do
  a' <- aeval a st
  return (insert x a' st)
cevalN n (Seq c1 c2) st = do
  st' <- cevalN (n-1) c1 st
  cevalN (n-1) c2 st'
cevalN n (If b c1 c2) st = do
  cond <- beval b st
  cevalN (n-1) (if cond then c1 else c2) st
cevalN n (While b c) st = do
  cond <- beval b st
  if cond then do
    st' <- cevalN (n-1) c st
    cevalN (n-1) (While b c) st'
  else
    return st
