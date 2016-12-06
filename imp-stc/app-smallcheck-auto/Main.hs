{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
 #-}

module Main where

import IMP
import Sec
import Properties

import qualified Bug1 as B1
import qualified Bug2 as B2

import Data.Maybe
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as Map

import Unbound.LocallyNameless hiding (Nil)

import System.Clock hiding (sec)
import System.Environment

import Test.SmallCheck
import Test.SmallCheck.Series

assocToVars :: Alpha a => a -> [b] -> Map Na b
assocToVars prog = go Map.empty (S.toList (fv prog :: S.Set Na))
  where go st [] _ = st
        go st (x:xs) (v:vs) = go (Map.insert x v st) xs vs

instance Monad m => Serial m (Name AExp) where
  series = cons0 (s2n "a") \/ cons0 (s2n "ab")

instance Monad m => Serial m AExp
instance Monad m => Serial m BExp
instance Monad m => Serial m Com

prop_confinement :: Monad m => SecTypeChecker -> Int -> Property m
prop_confinement tc limit =
  forAll $ \c ->
  forAll $ \(NonNegative l) ->
  changeDepth (const (numOfFreeVars c + 1)) $
  forAll $ \ss vs ->
    let ctx = assocToVars c ss
        s = assocToVars c vs
        s' = cevalN limit c s
    in
      length ss == numOfFreeVars c && all (>= 0) ss &&
      length vs == numOfFreeVars c &&
      isJust s' && fromMaybe False (tc ctx c l) ==> confinementCheck tc ctx c s l limit
  where numOfFreeVars c = S.size (fv c :: S.Set Na)

prop_noninterference :: Monad m => SecTypeChecker -> Int -> Property m
prop_noninterference tc limit =
  forAll $ \c ->
  forAll $ \(NonNegative l) ->
  changeDepth (const (numOfFreeVars c + 1)) $
  forAll $ \ss vs1 vs2 ->
    let ctx = assocToVars c ss
        s = assocToVars c vs1
        t = assocToVars c vs2
        s' = cevalN limit c s
        t' = cevalN limit c t
    in
      length ss == numOfFreeVars c && all (>= 0) ss &&
      length vs1 == numOfFreeVars c &&
      length vs2 == numOfFreeVars c &&
      isJust s' && isJust t' && fromMaybe False (tc ctx c 0) && eqLE ctx s t l ==> noninterferenceCheck tc ctx c s t l limit
  where numOfFreeVars c = S.size (fv c :: S.Set Na)

main :: IO ()
main = do
  args <- getArgs
  let properties = Map.fromList [("bug1#confinement", prop_confinement B1.secCheck),
                                 ("bug1#non-interference", prop_noninterference B1.secCheck),
                                 ("bug2#confinement", prop_confinement B2.secCheck),
                                 ("bug2#non-interference", prop_noninterference B2.secCheck)]
  if length args == 0
    then putStrLn "Please, provide the property to test!"
    else let depth = if length args == 2 then read $ args !! 1 else 3
         in case Map.lookup (head args) properties of
              Nothing -> putStrLn "You provided an invalid property!"
              Just property -> do start <- getTime Monotonic
                                  smallCheck depth (property 1000)
                                  end <- getTime Monotonic
                                  putStr "TIME:"
                                  print $ toNanoSecs $ diffTimeSpec start end
                                  putStrLn ""
