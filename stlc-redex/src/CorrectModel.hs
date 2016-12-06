module CorrectModel
  ( correctModel
  ) where

import qualified STLC as STLC 
import STLC hiding (isValue, isError, genericStep, genericTypeCheck, constantTy)

import Unbound.LocallyNameless hiding (Nil)

correctModel :: STLC.Model
correctModel = STLC.Model { STLC.isValue = isValue
                          , STLC.isError = isError
                          , STLC.genericStep = genericStep
                          , STLC.constantTy = constantTy
                          , STLC.genericTypeCheck = genericTypeCheck
                          }

isValue :: Exp -> Bool
isValue (Const _) = True
isValue (Lam _ _) = True
isValue (App (Const Cons) v) = isValue v
isValue (App (App (Const Cons) v1) v2) = isValue v1 && isValue v2
isValue (App (Const Plus) v) = isValue v
isValue _ = False

isError :: Exp -> Bool
isError (App (Const Hd) (Const Nil)) = True 
isError (App (Const Tl) (Const Nil)) = True 
isError (App e1 e2) = isError e1 || isError e2
isError _ = False

genericStep :: (Exp -> Bool) -> Exp -> Maybe Exp
genericStep isVal = runFreshM . step'
  where step' (App (Const Hd) (App (App (Const Cons) v) vs))
          | isVal v && isVal vs = return $ Just v
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

constantTy :: Constant -> Ty
constantTy (IntV _) = TyInt
constantTy Nil = TyList
constantTy Cons = TyFun TyInt (TyFun TyList TyList)
constantTy Hd = TyFun TyList TyInt
constantTy Tl = TyFun TyList TyList
constantTy Plus = TyFun TyInt (TyFun TyInt TyInt)

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
            Just (TyFun a b) -> case t2 of
              Just a' -> return $ if a == a' then Just b else Nothing
              Nothing -> return Nothing
            _ -> return Nothing
