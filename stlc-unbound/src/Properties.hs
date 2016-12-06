module Properties where

import STLC

import Unbound.LocallyNameless

wellTyped :: Exp -> Ty -> Bool
wellTyped e ty = runLFreshM (tc [] e ty)

-- Progress and Preservation for red

progressCheck :: Exp -> Bool
progressCheck e = isValue e || canStep
  where isValue (Lam _) = True
        isValue (Lit _) = True
        isValue EUnit   = True
        isValue _ = False
        canStep = runLFreshM $ not . (e `aeq`) <$> red e

preservationCheck :: Exp -> Ty -> Bool
preservationCheck e ty = runLFreshM $ do
  e' <- red e
  if wellTyped e ty 
    then return $ wellTyped e' ty
    else return False

-- Confluence of the reflexive and transitive closure of red (lemma 6.2.1)
-- if r ⇒ ∗ s and r ⇒ ∗ t then there exists some term u such that s ⇒ ∗ u and t ⇒ ∗ u.

confluenceCheck :: Exp -> Bool
confluenceCheck =
  -- forall s,t s.t r =>* s and r =>* t there exists u=u1=u2 s.t. s =>* u1 and t =>* u2
  all (\(s,t) -> any equalComponents [(u1,u2) | u1 <- reductionsOf s, u2 <- reductionsOf t]) . reductionPairsOf
  where reductionsOf = takeUntilRepeat . iterate (runLFreshM . red)
        reductionPairsOf r = let reds = reductionsOf r in [(s,t) | s <- reds, t <- reds]
        equalComponents (a,b) = a == b

instance Eq Exp where
  (==) = aeq

takeUntilRepeat :: Eq a => [a] -> [a]
takeUntilRepeat = collect []
  where collect acc [] = reverse acc
        collect acc [x] = reverse (x:acc)
        collect acc (x:y:xs) | x == y = reverse (x:acc)
                             | otherwise = collect (x:acc) (y:xs)

-- checks for validating equivalence relations (algeq, patheq)

isReflAlgeqCheck :: Exp -> Ty -> Bool
isReflAlgeqCheck e ty = runLFreshM (algeq e e ty)

isSymAlgeqCheck :: Exp -> Exp -> Ty -> Bool
isSymAlgeqCheck e1 e2 ty
  | runLFreshM (algeq e1 e2 ty) = runLFreshM (algeq e2 e1 ty)
  | otherwise = True

isTransAlgeqCheck :: Exp -> Exp -> Exp -> Ty -> Bool
isTransAlgeqCheck e1 e2 e3 ty
  | runLFreshM (algeq e1 e2 ty) && runLFreshM (algeq e2 e3 ty) = runLFreshM (algeq e1 e3 ty)
  | otherwise = True

isReflPatheqCheck :: Exp -> Bool
isReflPatheqCheck e = runLFreshM (patheq e e)

isSymPatheqCheck :: Exp -> Exp -> Bool
isSymPatheqCheck e1 e2
  | runLFreshM (patheq e1 e2) = runLFreshM (patheq e2 e1)
  | otherwise = True

isTransPatheqCheck :: Exp -> Exp -> Exp -> Bool
isTransPatheqCheck e1 e2 e3
  | runLFreshM (patheq e1 e2) && runLFreshM (patheq e2 e3) = runLFreshM (patheq e1 e3)
  | otherwise = True

-- Equivalence of algeq and redcomp on closed terms
-- (it should be false if we do not restrict ourselves to terms that do not have the unit type)

algeqEquivRedcompCheck :: Exp -> Exp -> Ty -> Bool
algeqEquivRedcompCheck e1 e2 ty = runLFreshM (algeq e1 e2 ty) == runLFreshM (redcomp e1 e2)
