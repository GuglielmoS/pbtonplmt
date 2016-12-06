module Correct where

import IMP
import Sec

secCheck :: SecContext -> Com -> Level -> Maybe Bool
secCheck _ Skip         _ = return True
secCheck ctx (Assign x a) l = do
  la <- sec ctx a
  lx <- sec ctx x
  return (la <= lx && l <= lx)
secCheck ctx (Seq c1 c2) l = do
  sc1 <- secCheck ctx c1 l
  sc2 <- secCheck ctx c2 l
  return (sc1 && sc2)
secCheck ctx (If b c1 c2) l = do
  lb <- sec ctx b
  sc1 <- secCheck ctx c1 (max lb l)
  sc2 <- secCheck ctx c2 (max lb l)
  return (sc1 && sc2)
secCheck ctx (While b c) l = do
  lb <- sec ctx b
  secCheck ctx c (max lb l)
