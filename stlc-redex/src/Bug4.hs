module Bug4
  ( bug4
  ) where

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)
import CorrectModel

bug4 :: STLC.Model
bug4 = correctModel { STLC.constantTy = constantTy } 

-- BUG: "the type of cons is incorrect"
constantTy :: Constant -> Ty
constantTy (IntV _) = TyInt
constantTy Nil = TyList
constantTy Cons = TyFun TyInt (TyFun TyList TyInt)
constantTy Hd = TyFun TyList TyInt
constantTy Tl = TyFun TyList TyList
constantTy Plus = TyFun TyInt (TyFun TyInt TyInt)
