{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Test.Feat
import Test.Feat.Enumerate

import System.Clock hiding (sec)
import System.Environment

assocToVars :: Alpha a => a -> [b] -> Map Na b
assocToVars prog = go Map.empty (S.toList (fv prog :: S.Set Na))
  where go st [] _ = st
        go st (x:xs) (v:vs) = go (Map.insert x v st) xs vs

instance Enumerable (Name AExp) where
  enumerate =  nullary (s2n "x")
            <> nullary (s2n "y")

-- generic instances
deriveEnumerable ''AExp
deriveEnumerable ''BExp
deriveEnumerable ''Com

prop_confinement :: SecTypeChecker -> Int -> (Com, Int, [Int], [Int]) -> Bool
prop_confinement tc limit (c,l,ss,vs) =
  let ctx = assocToVars c ss
      s = assocToVars c vs
      s' = cevalN limit c s
  in
    if l >= 0 &&
       length ss == numOfFreeVars && all (>= 0) ss &&
       length vs == numOfFreeVars &&
       isJust s' && fromMaybe False (tc ctx c l)
    then confinementCheck tc ctx c s l limit
    else True
  where numOfFreeVars = S.size (fv c :: S.Set Na)

prop_noninterference :: SecTypeChecker -> Int -> (Com, Int, [Int], [Int], [Int]) -> Bool
prop_noninterference tc limit (c,l,ss,vs1,vs2) =
  let ctx = assocToVars c ss
      s = assocToVars c vs1
      t = assocToVars c vs2
      s' = cevalN limit c s
      t' = cevalN limit c t
  in
    if l >= 0 &&
       length ss == numOfFreeVars && all (>= 0) ss &&
       length vs1 == numOfFreeVars &&
       length vs2 == numOfFreeVars &&
       isJust s' && isJust t' && fromMaybe False (tc ctx c 0) && eqLE ctx s t l
    then noninterferenceCheck tc ctx c s t l limit
    else True
  where numOfFreeVars = S.size (fv c :: S.Set Na)

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "Please, provide the property to test!"
    else let depth = if length args == 2 then read $ args !! 1 else 3
         in case head args of
              "bug1#confinement" -> do start <- getTime Monotonic
                                       featCheck depth (prop_confinement B1.secCheck 1000)
                                       end <- getTime Monotonic
                                       putStr "TIME:"
                                       print $ toNanoSecs $ diffTimeSpec start end
                                       putStrLn ""
              "bug1#non-interference" -> do start <- getTime Monotonic
                                            featCheck depth (prop_noninterference B1.secCheck 1000)
                                            end <- getTime Monotonic
                                            putStr "TIME:"
                                            print $ toNanoSecs $ diffTimeSpec start end
                                            putStrLn ""
              "bug2#non-interference" -> do start <- getTime Monotonic
                                            featCheck depth (prop_noninterference B2.secCheck 1000)
                                            end <- getTime Monotonic
                                            putStr "TIME:"
                                            print $ toNanoSecs $ diffTimeSpec start end
                                            putStrLn ""
              _ -> putStrLn "You provided an invalid property!"
