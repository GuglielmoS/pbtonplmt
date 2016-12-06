module IncrementalEnum
  ( wellTypedTerms
  , wellTypedTermsUpTo
  , termsUpTo
  ) where

import STLC

import Unbound.LocallyNameless hiding (Nil, enumerate)

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

vars :: [Name Exp]
vars = map (s2n . show) $ take 2 ['a'..'z']

terms :: [(Int, [Exp])]
terms = (0, []) : iterate enum (1, map Const constants ++ map Var vars)

enum :: (Int, [Exp]) -> (Int, [Exp])
enum (n, nterms) =
  let appSubterms = map (snd . (terms !!)) [1..n-1]
      appSubtermsRev = reverse appSubterms
      -- enumerate applications
      applications = concat $ zipWith combineApp appSubterms appSubtermsRev
      -- enumerate abstractions
      lambdas = [Lam ty (bind x st) | x <- vars, st <- nterms, ty <- types 1]
      -- aggreate
   in (n+1, applications ++ lambdas)
  where combineApp ts1 ts2 = [App t1 t2 | t1 <- ts1, t2 <- ts2]

wellTypedTerms :: Model -> Int -> [Exp]
wellTypedTerms stlc n = onlyWellTyped stlc $ snd (terms !! n)

wellTypedTermsUpTo :: Model -> Int -> [Exp]
wellTypedTermsUpTo stlc n = onlyWellTyped stlc $ concatMap snd $ take (n+1) terms

termsUpTo :: Int -> [Exp]
termsUpTo n = concatMap snd $ take (n+1) terms
