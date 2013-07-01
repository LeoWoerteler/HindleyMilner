
{-# LANGUAGE BangPatterns #-}

module HindleyMilner (
  LExpr(..),
  MonoType(..),
  PolyType(..),
  typeOf,
  parseLExpr,
  hindleyMilner
) where

import Debug.Trace             ( trace )
import Data.Maybe              ( fromMaybe )
import Data.Map                ( Map )
import Data.Set                ( Set, (\\) )
import Control.Applicative     ( (<$>), Applicative(..) )
import Control.Monad           ( when )
import Control.Monad.Trans     ( lift )
import Control.Monad.Identity  ( Identity, runIdentity )
import Control.Monad.Error     ( ErrorT,   runErrorT )
import Control.Monad.State     ( StateT,   runStateT, evalStateT, execStateT,
                                           get, put, modify )

import qualified Data.Map as M ( insert, empty, lookup, fold, keys )
import qualified Data.Set as S ( empty, union, toList, member, insert )

import HindleyMilner.Queue     ( Queue, fromList, enqueueAll, dequeue )
import HindleyMilner.LExpr     ( LExpr(..), VarName, typeOf, parseLExpr )
import HindleyMilner.Type      ( MonoType(..), PolyType(..),
                                 usedVars, replaceTVar, occursIn, tMapA )

type TVar       = Int
type ErrMsg     = String
type Equation   = (TVar, MonoType)
type Context    = Map TVar MonoType

type WithError  = ErrorT ErrMsg  Identity
type VarState   = StateT TVar    WithError
type Unificator = StateT Context VarState

hindleyMilner :: Map VarName PolyType -> LExpr a -> Either ErrMsg (LExpr PolyType)
hindleyMilner ext e = runIdentity . runErrorT . flip evalStateT 0 $ do
  e'          <- setIDs e
  let te'      = typeOf e'
  (!pt, ctx') <- flip runStateT M.empty $ do
    algorithmW ext e'
    polymorphize ext te'
  return $! insertTypes ctx' te' pt e'
  where
    insertTypes :: Context -> TVar -> PolyType -> LExpr TVar -> LExpr PolyType
    insertTypes ctx r pt = fmap $ \i ->
      if i == r then pt else Poly [] . fromMaybe (TVar i) $ M.lookup i ctx

algorithmW :: Map VarName PolyType -> LExpr TVar -> Unificator ()
algorithmW m (Var v i) =
  do
    t' <- lift $ case M.lookup v m of
      Nothing           -> fail $ "Free variable '" ++ v ++ "'."
      Just (Poly vs t') -> foldr rename (return t') vs
    addAssumption i t'
algorithmW m (Lam x e i) =
  do
    ix <- lift nextID
    addAssumption i $ TVar ix :-> TVar (typeOf e)
    algorithmW (M.insert x (Poly [] $ TVar ix) m) e
algorithmW m (Let x e e' i) =
  do
    ix <- lift nextID
    addAssumption ix $ TVar (typeOf e)
    algorithmW (M.insert x (Poly [] $ TVar ix) m) e
    !pt <- polymorphize m ix
    addAssumption i $ TVar (typeOf e')
    algorithmW (M.insert x pt m) e'
algorithmW m (App e e' i) =
  do
    addAssumption (typeOf e) $ TVar (typeOf e') :-> TVar i
    algorithmW m e
    algorithmW m e'

addAssumption :: TVar -> MonoType -> Unificator ()
addAssumption v a = get >>= (lift . lift . unifyEq v a) >>= put

rename :: TVar -> VarState MonoType -> VarState MonoType
rename v mt = nextID >>= \v' -> rename' v' <$> mt
  where rename' v' = replaceTVar $ \w -> TVar $ if v == w then v' else w

unifyEq :: TVar -> MonoType -> Context -> WithError Context
unifyEq v0 (TVar w) ctx0 | v0 == w = return ctx0
unifyEq v0 a0       ctx0 = trace ("add: T" ++ show v0 ++ " = " ++ show a0) $
                             go ctx0 $ fromList [(v0,a0)]
  where
    go :: Context -> Queue (TVar, MonoType) -> WithError Context
    go ctx q = case dequeue q of
      Nothing          -> return ctx
      Just ((v,a), q') -> do
        when (v `occursIn` a) $
          fail $ "Occurs check: " ++ show v ++ " = " ++ show (Poly [] a)
        case M.lookup v ctx of
          Nothing -> go (M.insert v a ctx) q'
          Just b  -> do
            es <- unifyTypes a b
            go ctx $ enqueueAll es q'

unifyTypes :: MonoType -> MonoType -> WithError [Equation]
unifyTypes a0 b0 =
    (\res -> trace ("unify " ++ show (a0,b0) ++ " = " ++ show res) res) <$>
    unify [] a0 b0
  where
    unify e (TVar a)       (TVar b)        | a == b    = return e
    unify e (TVar a)       t               = return ((a,t):e)
    unify e t              (TVar b)        = return ((b,t):e)
    unify e (a :-> b)      (a' :-> b')     = unify e a a' >>= \f -> unify f b b'
    unify e a@(TCons m ts) b@(TCons n ts') | m /= n    = failUni a b
                                           | otherwise = unifyAll e ts ts'
      where
        unifyAll e' []     []     = return e'
        unifyAll e' (x:xs) (y:ys) = unify e' x y >>= \e'' -> unifyAll e'' xs ys
        unifyAll _  _      _      = failUni a b
    unify _ a              b               = failUni a b

    failUni a b = fail $ "Types '" ++ show a ++ "' and '"
                                   ++ show b ++ "' cannot be unified."

polymorphize :: Map VarName PolyType -> TVar -> Unificator PolyType
polymorphize m ix = output <$> do
  ctx <- get >>= lift . lift . inlineTypes
  put ctx
  return $ poly ctx
  where
    poly :: Context -> PolyType
    poly ctx = Poly (S.toList $ used (Poly [] t) \\ outer) t
      where
        t      = mono ix
        outer  = M.fold (S.union . used) S.empty m
        used p = usedVars $ case p of Poly v t' -> Poly v $ replaceTVar mono t'
        mono x = fromMaybe (TVar x) $ M.lookup x ctx
    output v = trace ("polymorphize: T" ++ show ix ++ " = " ++ show v) v

inlineTypes :: Context -> WithError Context
inlineTypes ctx = flip execStateT M.empty $ mapM_ (dfs S.empty) $ M.keys ctx
  where
    dfs :: Set TVar -> TVar -> StateT Context WithError MonoType
    dfs open v = do
      closed <- get
      case M.lookup v closed of
        Just t' -> return t'
        Nothing -> do
          when (v `S.member` open) $ fail $ "Occurs check: " ++ show (TVar v)
          case M.lookup v ctx of
            Nothing -> return $ TVar v
            Just t  -> do
              t' <- tMapA (dfs $ S.insert v open) t
              modify $ M.insert v t'
              return t'

nextID :: VarState TVar
nextID = get >>= \k -> (put $! k+1) >> return k

setIDs :: LExpr a -> VarState (LExpr TVar)
setIDs (Var v _)      =
  Var v <$> nextID
setIDs (Lam x e _)    = nextID >>= \t ->
  Lam x <$> setIDs e <*> pure t
setIDs (Let x e e' _) = nextID >>= \t ->
  Let x <$> setIDs e <*> setIDs e' <*> pure t
setIDs (App e e' _)   = nextID >>= \t ->
  App   <$> setIDs e <*> setIDs e' <*> pure t

