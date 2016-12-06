module Gen
  ( expGen
  , wellTypedTermsOf
  ) where

import STLC

import Unbound.LocallyNameless hiding (Nil)
import Data.Set as S
import Control.Monad.Trans 

import Test.QuickCheck (Gen, suchThat)
import QuickCheck.GenT hiding (suchThat)

expGen' :: GenT FreshM Exp
expGen' = sized (gen S.empty)
  where gen vars 0 =
          if S.size vars == 0
          then constantsGen
          else oneof [Var <$> elements (S.toList vars),
                      constantsGen]
        gen vars n = oneof [constantsGen, lamGen vars n, appGen vars n]

        constantsGen = do
          c <- elements [IntV 0, Nil, Cons, Hd, Tl, Plus]
          return $ Const c

        lamGen vars n = do
          x <- lift $ fresh (string2Name "x")
          body <- gen (S.insert x vars) (n `div` 2)
          ty <- elements [TyInt, TyList]
          return $ Lam ty (bind x body)

        appGen vars n =
          let subexpGen = gen vars (n `div` 2) in do
            e1 <- subexpGen
            e2 <- subexpGen
            return $ App e1 e2

expGen :: Gen Exp
expGen = runFreshM <$> runGenT expGen'

wellTypedTermsOf :: Model -> Gen Exp
wellTypedTermsOf stlc = expGen `suchThat` (isWellTyped stlc)
