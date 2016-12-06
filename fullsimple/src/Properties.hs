module Properties where

import Syntax
import Typing
import Evaluator
import TaplError
import SimpleContext

import Data.Maybe
import Control.Monad.State
import Control.Monad.Error

--
-- Helpers
--

wellTyped :: Term -> Bool
wellTyped e = case runContextThrows (typeof e) of
  Left _ -> False
  Right _ -> True

extractDefault :: a -> ThrowsError a -> a
extractDefault _ (Right val) = val
extractDefault defaultVal _ = defaultVal

checkTy :: Term -> Ty -> ContextThrowsError Bool
checkTy e expectedTy = do
  ty <- typeof e
  pure $ ty == expectedTy

checkVariantTy :: [(String,Ty)] -> String -> Term -> ContextThrowsError Bool
checkVariantTy fieldsTy label term =
  case lookup label fieldsTy of
    Nothing -> pure False
    Just ty -> checkTy term ty

checkRecordTy :: [(String,Term)] -> [(String,Ty)] -> ContextThrowsError Bool
checkRecordTy fields fieldsTy = do
  tys <- mapM (typeof . snd) fields
  let typedLabels = zip (map fst fields) tys
  pure $ all (\(field, ty) -> case lookup field fieldsTy of
                                Nothing -> False
                                Just ty' -> ty == ty') typedLabels

--
-- Properties
--

preservationCheck :: Term -> Bool
preservationCheck e = extractDefault True $ runContextThrows $ do
  ty <- typeof e
  stepRes <- eval1 e
  case stepRes of
    Nothing -> return True
    Just e' -> do
      ty' <- typeof e'
      return (ty == ty')

progressCheck :: Term -> Bool
progressCheck e
  | wellTyped e = extractDefault True $ runContextThrows $ do
      e' <- eval1 e
      pure $ case e' of
        Nothing -> isval e
        Just _ -> True
  | otherwise = True

weakeningCheck :: Term -> Ty -> Bool
weakeningCheck e varTy = extractDefault True $ runContextThrows $ do
  ty <- typeof e
  withBinding "x" (VarBind varTy) $ do
    ty' <- typeof e
    pure (ty == ty')

canonicalFormsCheck :: Term -> Bool
canonicalFormsCheck e
  | isval e = extractDefault True $ runContextThrows $ do
      ty <- typeof e
      case (ty, e) of
        (TyBool, TmTrue) -> pure True
        (TyBool, TmFalse) -> pure True
        (TyUnit, TmUnit) -> pure True
        (TyFloat, TmFloat _) -> pure True
        (TyString, TmString _) -> pure True
        (TyNat, TmZero) -> pure True
        (TyNat, TmSucc e') -> checkTy e' TyNat
        (TyArr ty1 ty2, TmAbs x tyIn body)
          | ty1 == tyIn -> withBinding x (VarBind tyIn) $ checkTy body ty2
        (TyRecord fieldsTy, TmRecord fields) -> checkRecordTy fields fieldsTy
        (TyVariant fieldsTy, TmTag label term variantTy)
          | variantTy == ty -> checkVariantTy fieldsTy label term
        (ty', TmAscribe t ty'')
          | ty' == ty'' -> checkTy t ty'
        _ -> pure False
  | otherwise = True

inversionCheck :: Term -> Bool
inversionCheck = extractDefault True . runContextThrows . inversionCheck'

inversionCheck' :: Term -> ContextThrowsError Bool
inversionCheck' e@TmTrue = checkTy e TyBool
inversionCheck' e@TmFalse = checkTy e TyBool
inversionCheck' e@(TmAbs x ty body) = do
  eTy <- typeof e
  withBinding x (VarBind ty) $
    case eTy of
      TyArr t1 t2 | t1 == ty -> checkTy body t2
      _ -> pure False
inversionCheck' e@(TmApp e1 e2) = do
  outputTy <- typeof e
  inputTy <- typeof e2
  checkTy e1 (TyArr inputTy outputTy)
inversionCheck' e@(TmString _) = checkTy e TyString
inversionCheck' e@(TmFloat _) = checkTy e TyFloat
inversionCheck' e@(TmTimesFloat e1 e2) = do
  c1 <- checkTy e TyFloat
  c2 <- checkTy e1 TyFloat
  c3 <- checkTy e2 TyFloat
  pure $ c1 && c2 && c3
inversionCheck' e@TmZero = checkTy e TyNat
inversionCheck' e@(TmSucc e') = (&&) <$> checkTy e TyNat <*> checkTy e' TyNat
inversionCheck' e@(TmPred e') = (&&) <$> checkTy e TyNat <*> checkTy e' TyNat
inversionCheck' e@(TmIsZero e') = (&&) <$> checkTy e TyBool <*> checkTy e' TyNat
inversionCheck' e@(TmAscribe e' ty) = do
  t <- typeof e'
  if t == ty then checkTy e ty else pure False
inversionCheck' e@TmUnit = checkTy e TyUnit
inversionCheck' e@(TmLet x e1 e2) = do
  letTy <- typeof e
  e1Ty <- typeof e1
  withBinding x (VarBind e1Ty) $
    checkTy e2 letTy
inversionCheck' e@(TmRecord fields) = do
  recordTy <- typeof e
  case recordTy of
    TyRecord expectedFieldsTy ->
      and <$> zipWithM checkTy (map snd fields) (map snd expectedFieldsTy)
    _ -> pure False
inversionCheck' e@(TmProj rcd label) = do
  projTy <- typeof e
  rcdTy <- typeof rcd
  pure $ case rcdTy of
    TyRecord fieldsTy ->
      case lookup label fieldsTy of
        Just fieldTy -> fieldTy == projTy
        Nothing -> False
    _ -> False
inversionCheck' e@(TmFix e') = do
  fixTy <- typeof e
  checkTy e' (TyArr fixTy fixTy)
inversionCheck' e@(TmVar idx _) = do
  eTy <- typeof e
  ctx <- get
  binding <- liftThrows $ bindingOf idx ctx
  varTy <- liftThrows $ typeOfBinding binding
  pure $ varTy == eTy
inversionCheck' e@(TmCase variant cases) = do
  caseTy <- typeof e
  variantTy <- typeof variant
  case variantTy of
    TyVariant fieldsTy -> do
      cs <- mapM (\(l, (x,v)) -> case lookup l fieldsTy of
                                   Just fieldTy -> withBinding x (VarBind fieldTy) $ checkTy v caseTy
                                   Nothing -> pure False) cases
      pure $ and cs && all isJust (map (\(l, (_,_)) -> lookup l fieldsTy) cases)
    _ -> pure False
inversionCheck' e@(TmTag label e' ty) = do
  tagTy <- typeof e
  fieldTy <- typeof e'
  case tagTy of
    TyVariant fieldsTy
      | tagTy == ty -> case lookup label fieldsTy of
                         Nothing -> pure False
                         Just labelTy -> pure $ fieldTy == labelTy
    _ -> pure False
inversionCheck' e@(TmIf cond conseq alt) = do
  ifTy <- typeof e
  c1 <- checkTy cond TyBool
  c2 <- checkTy conseq ifTy
  c3 <- checkTy alt ifTy
  pure $ c1 && c2 && c3
