module SanityCheckSpec (spec) where

import Test.Hspec

import IMP
import Sec
import Properties

import Data.Map as Map

import qualified Bug1 as B1
import qualified Bug2 as B2

import Unbound.LocallyNameless hiding (Nil)

spec = do
  describe "Bug#1" bug1check
  describe "Bug#2" bug2check

bug1check = do
  let bug1a = Seq Skip (Assign (s2n "a") (N 0))
  it "Confinement shouldn't hold at level 1 for: skip; a := 0 (ENV: a=1, SEC: a=0)" $ do
    bug1a `shouldNotSatisfy`
      \c -> let s = Map.fromList [(s2n "a", 1)]
                ctx = Map.fromList [(s2n "a", 0)]
            in confinementCheck B1.secCheck ctx c s 1 1000

  let bug1b = Seq Skip (Assign (s2n "ab") (Var (s2n "a")))
  it "Noninterference should't hold at level 0 for: skip; ab = a (ENV1: a=0, ab=0 | ENV2: a=1, ab=0, SEC: a=1, ab=0)" $ do
    bug1b `shouldNotSatisfy`
      \c -> let s = Map.fromList [(s2n "a", 0), (s2n "ab", 0)]
                s' = Map.fromList [(s2n "a", 1), (s2n "ab", 0)]
                ctx = Map.fromList [(s2n "a", 1), (s2n "ab", 0)]
            in noninterferenceCheck B1.secCheck ctx c s s' 0 1000

bug2check = do
  let bug2 = Assign (s2n "ab") (Var (s2n "a"))
  it "Noninterference should't hold at level 0 for: ab = a (ENV1: a=0, ab=0 | ENV2: a=1, ab=0, SEC: a=1, ab=0)" $ do
    bug2 `shouldNotSatisfy`
      \c -> let s = Map.fromList [(s2n "a", 0), (s2n "ab", 0)]
                s' = Map.fromList [(s2n "a", 1), (s2n "ab", 0)]
                ctx = Map.fromList [(s2n "a", 1), (s2n "ab", 0)]
            in noninterferenceCheck B2.secCheck ctx c s s' 0 1000
