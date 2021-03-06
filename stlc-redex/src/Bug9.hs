module Bug9
  ( bug9
  ) where

import CorrectModel

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)

import Unbound.LocallyNameless hiding (Nil)

bug9 :: STLC.Model
bug9 = correctModel { STLC.genericTypeCheck = genericTypeCheck }

-- BUG:  "variables aren't required to match in lookup"
genericTypeCheck :: (Constant -> Ty) -> Exp -> Context -> Maybe Ty
genericTypeCheck constTy e = runFreshM . typeCheck' e
  where typeCheck' (Const c) _ = return $ Just (constTy c)
        typeCheck' (Var x) context =
          case lookup x context of
            Nothing -> return Nothing
            Just _ -> return $ Just (snd $ last context)
        typeCheck' (Lam t1 bnd) context = do
          (x, body) <- unbind bnd
          ty <- typeCheck' body ((x,t1) : context)
          case ty of
            Just t2 -> return $ Just (TyFun t1 t2)
            Nothing -> return Nothing
        typeCheck' (App e1 e2) context = do
          t1 <- typeCheck' e1 context
          t2 <- typeCheck' e2 context
          case t1 of
            Just (TyFun a b) -> case t2 of
              Just a' -> return $ if a == a' then Just b else Nothing
              Nothing -> return Nothing
            _ -> return Nothing
