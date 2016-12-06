{-# LANGUAGE TemplateHaskell #-}

module TestFeat where

import Syntax
import Properties

import Test.Feat
import Test.Feat.Enumerate

deriveEnumerable ''Ty
deriveEnumerable ''Term
