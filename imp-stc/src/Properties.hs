module Properties where

import IMP
import Sec

import Data.Maybe

-- executable properties

antiMonotonicityCheck :: SecTypeChecker -> SecContext -> Com -> Level -> Level -> Bool
antiMonotonicityCheck tc ctx c l l' =
  if fromMaybe False (tc ctx c l) && l <= l' then
    fromMaybe False (tc ctx c l')
  else
    True

confinementCheck :: SecTypeChecker -> SecContext -> Com -> State -> Level -> Int -> Bool
confinementCheck tc ctx c s l limit =
  case cevalN limit c s of
    Nothing -> True
    Just t  -> if fromMaybe False (tc ctx c l) then
                 eqLT ctx s t l
               else
                 True

noninterferenceCheck :: SecTypeChecker -> SecContext -> Com -> State -> State -> Level -> Int -> Bool
noninterferenceCheck tc ctx c s t l limit =
  case (cevalN limit c s, cevalN limit c t) of
    (Just s', Just t') -> if fromMaybe False (tc ctx c 0) && eqLE ctx s t l then
                            eqLE ctx s' t' l
                          else
                            True

    (_, _) -> True
