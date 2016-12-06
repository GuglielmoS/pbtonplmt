module Bug1
  ( bug1
  ) where

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)
import CorrectModel

import Unbound.LocallyNameless hiding (Nil)

bug1 :: STLC.Model
bug1 = correctModel { STLC.genericTypeCheck = genericTypeCheck }

-- BUG: "app rule the range of the function is matched to the argument"
genericTypeCheck :: (Constant -> Ty) -> Exp -> Context -> Maybe Ty
genericTypeCheck constTy e = runFreshM . typeCheck' e
  where typeCheck' (Const c) _ = return $ Just (constTy c)
        typeCheck' (Var x) context = return $ lookup x context
        typeCheck' (Lam t1 bnd) context = do
          (x, body) <- unbind bnd
          ty <- typeCheck' body ((x,t1) : context)
          case ty of
            Just t2 -> return $ Just $ TyFun t1 t2
            Nothing -> return Nothing 
        typeCheck' (App e1 e2) context = do
          t1 <- typeCheck' e1 context
          t2 <- typeCheck' e2 context
          case t1 of
            Just (TyFun inputTy outputTy) -> case t2 of
              Just argumentTy -> return $ if argumentTy == outputTy then Just outputTy else Nothing
              Nothing -> return Nothing
            _ -> return Nothing
