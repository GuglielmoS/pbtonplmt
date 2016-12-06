module Bug6
  ( bug6
  ) where

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)
import CorrectModel

import Unbound.LocallyNameless hiding (Nil)

bug6 :: STLC.Model
bug6 = correctModel { STLC.genericStep = genericStep }

-- BUG: "hd reduction acts on partially applied cons"
genericStep :: (Exp -> Bool) -> Exp -> Maybe Exp
genericStep isVal = runFreshM . step'
  where step' (App (Const Hd) (App (Const Cons) v))
          | isVal v = return $ Just v
        step' (App (Const Hd) (Const Nil)) = return Nothing
        step' (App (Const Tl) (App (App (Const Cons) v) vs))
          | isVal v && isVal vs = return $ Just vs
        step' (App (Const Tl) (Const Nil)) = return Nothing
        step' (App (App (Const Plus) v1) v2)
          | isVal v1 && isVal v2 =
              case (v1, v2) of
                (Const (IntV n1), Const (IntV n2)) -> return $ Just (Const (IntV (n1+n2)))
                _ -> return Nothing
        step' (App (Lam _ bnd) e)
          | isVal e = do
            (x, body) <- unbind bnd
            return (Just $ subst x e body)
        step' (App e1 e2)
          | isVal e1 = do
              e2' <- step' e2
              case e2' of
                Just e -> return (Just $ App e1 e)
                Nothing -> return Nothing
          | otherwise = do
              e1' <- step' e1
              case e1' of
                Just e -> return (Just $ App e e2)
                Nothing -> return Nothing
        step' _ = return Nothing
