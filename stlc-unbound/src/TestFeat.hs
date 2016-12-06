{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module TestFeat where

import STLC
import Properties

import Unbound.LocallyNameless

import Test.Feat
import Test.Feat.Enumerate

instance Enumerable (Name Exp) where
  enumerate = nullary (s2n "a") <> nullary (s2n "b")

instance Enumerable (Bind (Name Exp) Exp) where
  enumerate = unary (funcurry bind)

-- for testing algeq equivalence to redcomp
{-
instance Enumerable Ty where
  enumerate = nullary TInt <> pay (unary (funcurry Arr))

instance Enumerable Exp where
  enumerate = pay (unary Lit) <>
              pay (unary Var) <>
              pay (unary Lam) <>
              pay (unary (funcurry (funcurry App)))
-}

newtype Path = Path Exp
  deriving (Show)

deriveEnumerable ''Ty
deriveEnumerable ''Exp

instance Enumerable Path where
  enumerate = pay (unary (Path . Lit)) <>
              pay (unary (Path . Var)) <>
              pay (unary (funcurry (funcurry (\e1 ty e2 -> Path $ App e1 ty e2))))

prop_progress :: (Exp,Ty) -> Bool
prop_progress (e,ty)
  | wellTyped e ty = progressCheck e
  | otherwise = True

prop_preservation :: (Exp,Ty) -> Bool
prop_preservation (e,ty)
  | wellTyped e ty = preservationCheck e ty
  | otherwise = True

prop_confluenceOfRed :: (Exp,Ty) -> Bool
prop_confluenceOfRed (e,ty)
  | wellTyped e ty = confluenceCheck e
  | otherwise = True

prop_isReflAlgeq :: (Exp,Ty) -> Bool
prop_isReflAlgeq (e,ty)
  | wellTyped e ty = isReflAlgeqCheck e ty
  | otherwise = True

prop_isSymAlgeq :: (Exp,Exp,Ty) -> Bool
prop_isSymAlgeq (e1,e2,ty)
  | wellTyped e1 ty && wellTyped e2 ty = isSymAlgeqCheck e1 e2 ty
  | otherwise = True

prop_isTransAlgeq :: (Exp,Exp,Exp,Ty) -> Bool
prop_isTransAlgeq (e1,e2,e3,ty)
  | wellTyped e1 ty && wellTyped e2 ty && wellTyped e3 ty = isTransAlgeqCheck e1 e2 e3 ty
  | otherwise = True

prop_isReflPatheq :: (Path,Ty) -> Bool
prop_isReflPatheq (Path e,ty)
  | wellTyped e ty = isReflPatheqCheck (runLFreshM $ wh e)
  | otherwise = True

prop_isSymPatheq :: (Path,Path,Ty) -> Bool
prop_isSymPatheq (Path e1,Path e2,ty)
  | wellTyped e1 ty && wellTyped e2 ty = isSymPatheqCheck (runLFreshM $ wh e1) (runLFreshM $ wh e2)
  | otherwise = True

prop_isTransPatheq :: (Path,Path,Path,Ty) -> Bool
prop_isTransPatheq (Path e1,Path e2,Path e3,ty)
  | wellTyped e1 ty && wellTyped e2 ty && wellTyped e3 ty =
      isTransPatheqCheck (runLFreshM $ wh e1) (runLFreshM $ wh e2) (runLFreshM $ wh e3)
  | otherwise = True

prop_algeqEquivRedcomp :: (Exp, Exp, Ty) -> Bool
prop_algeqEquivRedcomp (e1,e2,ty)
  | wellTyped e1 ty && wellTyped e2 ty = algeqEquivRedcompCheck e1 e2 ty
  | otherwise = True
