module FeatUtil where

import STLC
import Properties

import Data.List

progress :: Model -> Exp -> Bool
progress stlc e | isWellTyped stlc e = progressCheck stlc e
                | otherwise = True

preservation :: Model -> Exp -> Bool
preservation stlc e | isWellTyped stlc e = preservationCheck stlc e
                    | otherwise = True

findCounterExample :: [(Integer, [Exp])] -> (Exp -> Bool) -> Int -> Maybe Exp
findCounterExample values prop limit = search (take (limit+1) values)
  where search [] = Nothing
        search ((_, candidates):rest) =
          case find (not . prop) candidates of
            Just cex -> return cex
            Nothing -> search rest
