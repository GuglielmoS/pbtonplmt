module SmallCheckUtil where

import STLC
import Properties

import Test.SmallCheck

progress :: Monad m => Model -> Exp -> Property m
progress stlc e = isWellTyped stlc e ==> progressCheck stlc e

preservation :: Monad m => Model -> Exp -> Property m
preservation stlc e = isWellTyped stlc e && canStep stlc e ==> preservationCheck stlc e
