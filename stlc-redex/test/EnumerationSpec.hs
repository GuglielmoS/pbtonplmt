module EnumerationSpec (spec) where

import Test.Hspec

import STLC
import CorrectModel
import NaiveEnum
import qualified IncrementalEnum as IE
import Properties

spec :: Spec
spec = do
  it "Terms enumeration with size produces only well types terms" $ do
    all (isWellTyped correctModel) $ enumUpTo 7 (wellTypedTerms correctModel)
  it "Terms enumeration with size produces terms of the correct size" $
    and $ flip map [1..7] $ \size ->
      (all ((== size) . expSize) $ wellTypedTerms correctModel size)
  it "Terms enumeration with depth produces only well types terms" $ do
    all (isWellTyped correctModel) $ enumUpTo 3 (wellTypedTermsD correctModel)
  it "Terms enumeration with depth produces terms of the correct depth" $
    and $ flip map [1..3] $ \depth ->
      (all ((== depth) . expDepth) $ wellTypedTermsD correctModel depth)
  it "Incremental terms enumeration with size produces only well types terms" $ do
    all (isWellTyped correctModel) $ IE.wellTypedTermsUpTo correctModel 5
  it "Incremental terms enumeration with size produces terms of the correct size" $
    and $ flip map [1..5] $ \size ->
      (all ((== size) . expSize) $ IE.wellTypedTerms correctModel size)
