{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , DeriveGeneric
           , DeriveDataTypeable
  #-}

module STLC where

import Data.Maybe (isJust)

import Unbound.LocallyNameless hiding (Nil, generate, Generic)

import qualified GHC.Generics as GHC

data Ty = TyInt
        | TyList
        | TyFun Ty Ty
        deriving (Show, Eq, GHC.Generic)

data Constant = IntV Int
              | Nil
              | Cons
              | Hd
              | Tl
              | Plus
              deriving (Show, Eq, GHC.Generic)

data Exp = Const Constant
         | Var (Name Exp)
         | Lam Ty (Bind (Name Exp) Exp)
         | App Exp Exp
         deriving (Show, GHC.Generic)

$(derive [''Ty, ''Constant, ''Exp])

instance Alpha Ty
instance Alpha Constant
instance Alpha Exp
instance Subst Exp Ty where
instance Subst Exp Constant where
instance Subst Exp Exp where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

instance Eq Exp where
  (==) = aeq

type Context = [(Name Exp, Ty)]

data Model = Model { isValue :: Exp -> Bool
                   , isError :: Exp -> Bool
                   , genericStep :: (Exp -> Bool) -> Exp -> Maybe Exp
                   , constantTy :: Constant -> Ty
                   , genericTypeCheck :: (Constant -> Ty) -> Exp -> Context -> Maybe Ty
                   }

typeCheck :: Model -> Exp -> Context -> Maybe Ty
typeCheck stlc = genericTypeCheck stlc (constantTy stlc) 

step :: Model -> Exp -> Maybe Exp
step stlc = genericStep stlc (isValue stlc)

isWellTyped :: Model -> Exp -> Bool
isWellTyped stlc e = isJust $ typeCheck stlc e []

canStep :: Model -> Exp -> Bool
canStep stlc = isJust . step stlc
