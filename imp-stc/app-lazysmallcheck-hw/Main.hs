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

import Test.LazySmallCheck

import Control.Exception

instance Serial (Name AExp) where
  series = cons0 (s2n "a") \/ cons0 (s2n "b")

instance Serial AExp where
  series = cons1 N \/
           cons1 Var \/
           cons2 Plus

instance Serial BExp where
  series = cons1 Bc \/
           cons1 Not \/
           cons2 And \/
           cons2 Less

instance Serial Com where
  series = cons0 Skip \/
           cons2 Assign \/
           cons3 If \/
           cons2 While

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

check prop = catchAny (test prop) $ \_ -> return ()

assocToVars :: Alpha a => a -> [b] -> Map Na b
assocToVars prog = go Map.empty (S.toList (fv prog :: S.Set Na))
  where go st [] _ = st
        go st (x:xs) (v:vs) = go (Map.insert x v st) xs vs

prop_confinement :: SecTypeChecker -> Int -> Com -> Int -> [Int] -> [Int] -> Property
prop_confinement tc limit c l ss vs =
  let ctx = assocToVars c ss
      s = assocToVars c vs
      s' = cevalN limit c s
  in
    lift (l >= 0) *&*
    lift (length ss == numOfFreeVars) *&*
    lift (all (>= 0) ss) *&*
    lift (length vs == numOfFreeVars) *&*
    lift (isJust s') *&*
    lift (fromMaybe False (tc ctx c l))
    *=>*
    lift (confinementCheck tc ctx c s l limit)
  where numOfFreeVars = S.size (fv c :: S.Set Na)

prop_noninterference :: SecTypeChecker -> Int -> Com -> Int -> [Int] -> [Int] -> [Int] -> Property
prop_noninterference tc limit c l ss vs1 vs2 =
  let ctx = assocToVars c ss
      s = assocToVars c vs1
      t = assocToVars c vs2
      s' = cevalN limit c s
      t' = cevalN limit c t
  in
    lift (l >= 0) *&*
    lift (length ss == numOfFreeVars) *&*
    lift (all (>= 0) ss) *&*
    lift (length vs1 == numOfFreeVars) *&*
    lift (length vs2 == numOfFreeVars) *&*
    lift (isJust s') *&*
    lift (isJust t') *&*
    lift (fromMaybe False (tc ctx c 0)) *&*
    lift (eqLE ctx s t l)
    *=>*
    lift (noninterferenceCheck tc ctx c s t l limit)
  where numOfFreeVars = S.size (fv c :: S.Set Na)

prop_confinementWithoutPC :: SecTypeChecker -> Int -> Com -> Int -> [Int] -> [Int] -> Bool
prop_confinementWithoutPC tc limit c l ss vs =
  let ctx = assocToVars c ss
      s = assocToVars c vs
      s' = cevalN limit c s
  in
    l >= 0 &&
    length ss == numOfFreeVars &&
    all (>= 0) ss &&
    length vs == numOfFreeVars &&
    isJust s' &&
    fromMaybe False (tc ctx c l)
    ==>
    confinementCheck tc ctx c s l limit
  where numOfFreeVars = S.size (fv c :: S.Set Na)

prop_noninterferenceWithoutPC :: SecTypeChecker -> Int -> Com -> Int -> [Int] -> [Int] -> [Int] -> Bool
prop_noninterferenceWithoutPC tc limit c l ss vs1 vs2 =
  let ctx = assocToVars c ss
      s = assocToVars c vs1
      t = assocToVars c vs2
      s' = cevalN limit c s
      t' = cevalN limit c t
  in
    (l >= 0) &&
    (length ss == numOfFreeVars) &&
    (all (>= 0) ss) &&
    (length vs1 == numOfFreeVars) &&
    (length vs2 == numOfFreeVars) &&
    (isJust s') &&
    (isJust t') &&
    (fromMaybe False (tc ctx c 0)) &&
    (eqLE ctx s t l)
    ==>
    (noninterferenceCheck tc ctx c s t l limit)
  where numOfFreeVars = S.size (fv c :: S.Set Na)

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "Please, provide the property to test!"
    else let depth = if length args == 2 then read $ args !! 1 else 3
         in case head args of
              "bug1#confinement" -> do start <- getTime Monotonic
                                       check (prop_confinement B1.secCheck 1000)
                                       end <- getTime Monotonic
                                       putStr "TIME:"
                                       print $ toNanoSecs $ diffTimeSpec start end
                                       putStrLn ""
              "bug1#non-interference" -> do start <- getTime Monotonic
                                            check (prop_noninterference B1.secCheck 1000)
                                            end <- getTime Monotonic
                                            putStr "TIME:"
                                            print $ toNanoSecs $ diffTimeSpec start end
                                            putStrLn ""
              "bug2#non-interference" -> do start <- getTime Monotonic
                                            check (prop_noninterference B2.secCheck 1000)
                                            end <- getTime Monotonic
                                            putStr "TIME:"
                                            print $ toNanoSecs $ diffTimeSpec start end
                                            putStrLn ""
              "bug1#confinementNOPC" -> do start <- getTime Monotonic
                                           check (prop_confinementWithoutPC B1.secCheck 1000)
                                           end <- getTime Monotonic
                                           putStr "TIME:"
                                           print $ toNanoSecs $ diffTimeSpec start end
                                           putStrLn ""
              "bug2#non-interferenceNOPC" -> do start <- getTime Monotonic
                                                check (prop_noninterferenceWithoutPC B2.secCheck 1000)
                                                end <- getTime Monotonic
                                                putStr "TIME:"
                                                print $ toNanoSecs $ diffTimeSpec start end
                                                putStrLn ""
              _ -> putStrLn "You provided an invalid property!"
