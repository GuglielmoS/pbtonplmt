module Properties where

import STLC

import Unbound.LocallyNameless hiding (Nil)

import Data.Set as S

expSize :: Exp -> Int
expSize = runFreshM . getSize
  where getSize (Const _) = return 1
        getSize (Var _) = return 1
        getSize (Lam _ bnd) = do
          (_, body) <- unbind bnd
          bodySize <- getSize body
          return $ 1 + bodySize
        getSize (App e1 e2) = do
          s1 <- getSize e1
          s2 <- getSize e2
          return $ 1 + s1 + s2

expDepth :: Exp -> Int
expDepth = runFreshM . getDepth
  where getDepth (Const _) = return 1
        getDepth (Var _) = return 1
        getDepth (Lam _ bnd) = do
          (_, body) <- unbind bnd
          bodyDepth <- getDepth body
          return $ 1 + bodyDepth
        getDepth (App e1 e2) = do
          s1 <- getDepth e1
          s2 <- getDepth e2
          return $ 1 + max s1 s2

isClosed :: Exp -> Bool
isClosed e = S.size (fv e :: Set (Name Exp)) == 0

progressCheck :: Model -> Exp -> Bool
progressCheck stlc e = isValue stlc e || isError stlc e || canStep stlc e

preservationCheck :: Model -> Exp -> Bool
preservationCheck stlc e =
  case typeCheck stlc e [] of
    Nothing -> True
    Just t ->
      case step stlc e of
        Nothing -> True
        Just e' -> do
          case typeCheck stlc e' [] of
            Just t' -> t == t'
            Nothing -> False
