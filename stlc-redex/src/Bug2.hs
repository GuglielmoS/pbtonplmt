module Bug2
  ( bug2
  ) where

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)
import CorrectModel

bug2 :: STLC.Model
bug2 = correctModel { STLC.isValue = isValue }

-- BUG: "the ((cons v) v) value has been omitted"
isValue :: Exp -> Bool
isValue (Const _) = True
isValue (Lam _ _) = True
isValue (App (Const Cons) v) = isValue v
--isValue (App (App (Const Cons) v1) v2) = isValue v1 && isValue v2
isValue (App (Const Plus) v) = isValue v
isValue _ = False
