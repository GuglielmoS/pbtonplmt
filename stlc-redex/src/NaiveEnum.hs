module NaiveEnum
  ( constants
  , types
  , wellTypedTerms
  , wellTypedTermsD
  , enumUpTo
  , termsUpTo
  ) where

import STLC

import qualified Data.Set as S
import Unbound.LocallyNameless hiding (Nil)

constants :: [Constant] 
constants = [IntV 0, Cons, Nil, Hd, Tl, Plus]

types :: Int -> [Ty]
types 0 = []
-- base types
types 1 = [TyInt, TyList]
-- function types
types n
  | n > 1 =
    let subtypes = map types [1..n-2] in
      concat $ zipWith combineFun subtypes (reverse subtypes)
  | otherwise = error "size must be >= 0"
  where combineFun ts1 ts2 = [TyFun t1 t2 | t1 <- ts1, t2 <- ts2]

onlyWellTyped :: Model -> [Exp] -> [Exp]
onlyWellTyped stlc = filter (isWellTyped stlc)

terms :: Int -> S.Set (Name Exp) -> FreshM [Exp]
terms 0 _ = return []
terms 1 vars = return $ map Const constants ++ map Var (S.toList vars)
terms n vars
  | n > 1 = do
    -- enumerate applications
    appSubterms <- mapM (\s -> terms s vars) [1..n-2]
    revAppSubterms <- mapM (\s -> terms s vars) [n-2,n-3..1]
    let applications = concat $ zipWith combineApp appSubterms revAppSubterms
    -- enumerate abstractions
    x <- fresh (string2Name "x")
    subterms <- terms (n-1) (S.insert x vars)
    let lambdas = [Lam ty (bind x subterm) | subterm <- subterms, ty <- enumUpTo 1 types]
    -- aggregate
    return $ applications ++ lambdas
  | otherwise = error "size must be >= 0"
  where combineApp es1 es2 = [App e1 e2 | e1 <- es1, e2 <- es2]

termsD :: Model -> Int -> S.Set (Name Exp) -> FreshM [Exp]
termsD _ 1 vars = return $ map Const constants ++ map Var (S.toList vars)
termsD stlc n vars
  | n > 1 = do
    -- enumerate applications
    deepestSubterms <- termsD stlc (n-1) vars
    smallerSubterms <- mapM (\s -> termsD stlc s vars) [1..n-2]
    let allSubterms = concat $ deepestSubterms : smallerSubterms
    let applications = [App e1 e2 | e1 <- deepestSubterms, e2 <- allSubterms] ++
                       [App e2 e1 | e1 <- deepestSubterms, e2 <- allSubterms]
    -- enumerate abstractions
    x <- fresh (string2Name "x")
    subterms <- termsD stlc (n-1) (S.insert x vars)
    let lambdas = [Lam ty (bind x subterm) | subterm <- subterms, ty <- enumUpTo 1 types]
    -- aggregate
    return $ applications ++ lambdas
  | otherwise = error "depth must be >= 1"

wellTypedTerms :: Model -> Int -> [Exp]
wellTypedTerms stlc n = onlyWellTyped stlc $ runFreshM $ terms n S.empty

wellTypedTermsD :: Model -> Int -> [Exp]
wellTypedTermsD stlc n = onlyWellTyped stlc $ runFreshM $ termsD stlc n S.empty

enumUpTo :: Int -> (Int -> [a]) -> [a]
enumUpTo limit gen = concatMap gen [1..limit]

termsUpTo :: Int -> [Exp]
termsUpTo limit = concatMap (\n -> runFreshM $ terms n S.empty) [1..limit]
