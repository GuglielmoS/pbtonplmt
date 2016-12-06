module SanityCheckSpec (spec) where

import Test.Hspec

import STLC
import Properties
import Bug1
import Bug2
import Bug3
import Bug4
import Bug5
import Bug6
import Bug7
import Bug8
import Bug9

import Unbound.LocallyNameless hiding (Nil)

spec = do
  describe "Bug#1" bug1check
  describe "Bug#2" bug2check
  describe "Bug#3" bug3check
  describe "Bug#4" bug4check
  describe "Bug#5" bug5check
  describe "Bug#6" bug6check
  describe "Bug#7" bug7check
  describe "Bug#8" bug8check
  describe "Bug#9" bug9check

pprintBug :: Exp -> String
pprintBug exp = show exp ++ " [D=" ++ show (expDepth exp) ++ ", S=" ++ show (expSize exp) ++ "]"

bug1check = do
  let bug_prog = App (Const Hd) (Const (IntV 0))
  let bug_pres = App (Lam TyInt (bind (s2n "x") (Lam TyList (bind (s2n "y") (Var (s2n "x")))))) (Const Hd)
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug1
  it ("Preservation must fail with " ++ pprintBug bug_pres) $ do
    bug_pres `shouldNotSatisfy` preservationCheck bug1

bug2check = do
  let bug_prog = App (App (Const Cons) (Const (IntV 0))) (Const Nil)
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug2

bug3check = do
  let bug_prog = App (Const Hd) (Const (IntV 0))
  let bug_pres = App (Lam TyInt (bind (s2n "x") (Const Tl))) (Const Tl)
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug3
  it ("Preservation must fail with " ++ pprintBug bug_pres) $ do
    bug_pres `shouldNotSatisfy` preservationCheck bug3

bug4check = do
  let bug_prog = App (App (Const Plus) (Const (IntV 1))) (App (App (Const Cons) (Const (IntV 1))) (Const Nil))
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug4

bug5check = do
  let bug_pres = App (Const Tl) (App (App (Const Cons) (Const (IntV 0))) (Const Nil))
  it ("Preservation must fail with " ++ pprintBug bug_pres) $ do
    bug_pres `shouldNotSatisfy` preservationCheck bug5

bug6check = do
  let bug_prog = App (Const Hd) (App (App (Const Cons) (Const (IntV 0))) (Const Nil))
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug6

bug7check = do
  let bug_prog = App (Const Cons) (App (Lam TyInt (bind (s2n "x") (Const (IntV 0)))) (Const (IntV 0)))
  it ("Progress must fail with " ++ pprintBug bug_prog) $ do
    bug_prog `shouldNotSatisfy` progressCheck bug7

bug8check = do
  let bug_pres = App (Lam TyList (bind (s2n "x") (Var (s2n "x")))) (Const Nil)
  it ("Preservation must fail with " ++ pprintBug bug_pres) $ do
    bug_pres `shouldNotSatisfy` preservationCheck bug8

bug9check = do
  let bug_pres = App (Lam TyList (bind (s2n "x") (Lam TyInt (bind (s2n "y") (Var (s2n "y")))))) (Const Nil)
  it ("Preservation must fail with " ++ pprintBug bug_pres) $ do
    bug_pres `shouldNotSatisfy` preservationCheck bug9

