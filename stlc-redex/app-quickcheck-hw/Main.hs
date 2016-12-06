module Main where

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
import Gen

import Data.Maybe

import System.Clock
import System.Environment
import qualified Data.Map as Map

import Test.QuickCheck hiding (property)

prop_progressG :: Gen Exp -> Model -> Property
prop_progressG expressions stlc =
  forAll expressions $ \e ->
    collect (expSize e) $
    not (isValue stlc e) && not (isError stlc e) ==> canStep stlc e

prop_preservationG :: Gen Exp -> Model -> Property
prop_preservationG expressions stlc =
  forAll expressions $ \e ->
    collect (expSize e) $
    let ty = typeCheck stlc e [] in
    let e' = step stlc e in
      isJust ty && isJust e' ==>
        case typeCheck stlc (fromJust e') [] of
          Just ty' -> fromJust ty == ty'
          Nothing -> False

progress :: Model -> Property
progress stlc = prop_progressG (wellTypedTermsOf stlc) stlc

preservation :: Model -> Property
preservation stlc = prop_preservationG (wellTypedTermsOf stlc) stlc

config :: Args
config = stdArgs { maxSize = 10, maxDiscardRatio = 1000, maxSuccess = 100000}

main :: IO ()
main = do
  args <- getArgs
  let properties = Map.fromList [("bug1#progress", progress bug1), ("bug1#preservation", preservation bug1),
                                 ("bug2#progress", progress bug2),
                                 ("bug3#progress", progress bug3), ("bug3#preservation", preservation bug3),
                                 ("bug4#progress", progress bug4),
                                 ("bug5#preservation", preservation bug5),
                                 ("bug6#progress", progress bug6),
                                 ("bug7#progress", progress bug7),
                                 ("bug8#preservation", preservation bug8),
                                 ("bug9#preservation", preservation bug9)]
  if length args == 0
    then putStrLn $ "Please, specify the property to test!"
    else case Map.lookup (head args) properties of
           Nothing -> putStrLn "You specified an invalid property!"
           Just property -> do start <- getTime Monotonic
                               quickCheckWith config property
                               end <- getTime Monotonic
                               putStr "TIME:"
                               print $ toNanoSecs $ diffTimeSpec start end
                               putStrLn ""
