
module HindleyMilner (
  LExpr(..),
  MonoType(..),
  PolyType(..),
  typeOf,
  hindleyMilner
) where

import Debug.Trace ( trace )

import Data.Maybe              ( fromMaybe )
import Data.Map                ( Map )
import Data.Set                ( (\\) )
import Control.Applicative     ( (<$>), Applicative(..) )
import Control.Monad           ( when )
import Control.Monad.Trans     ( lift )
import Control.Monad.State     ( StateT, evalStateT, execStateT, get, put, modify )
import Control.Monad.Instances ( )

import qualified Data.Map as M ( insert, empty, foldrWithKey', lookup, fold, keys )
import qualified Data.Set as S ( empty, singleton, union, toList, member, insert )
import qualified Queue    as Q ( Queue, fromList, enqueueAll, dequeue )

import LExpr              ( LExpr(..), VarName, typeOf )
import HindleyMilner.Type ( MonoType(..), PolyType(..),
                            usedVars, replaceTVar, occursIn, tFoldA )

type TVar = Int

type Error      = String
type Equation   = (TVar, MonoType)
type Context    = Map TVar PolyType

type VarState   = StateT TVar (Either Error)
type Unificator = StateT Context VarState

hindleyMilner :: Map VarName PolyType -> LExpr a -> Either Error (LExpr PolyType)
hindleyMilner ext e = flip evalStateT 0 $ do
  (m,ctx) <- adoptVars ext
  e'      <- setIDs e
  ctx'    <- flip execStateT ctx $ do
    algorithmW m e'
    polymorphize m $ typeOf e'
  return $ insertTypes ctx' e'
  where
    insertTypes :: Context -> LExpr TVar -> LExpr PolyType
    insertTypes ctx = fmap $ \i -> fromMaybe (Mono $ TVar i) $ M.lookup i ctx

algorithmW :: Map VarName TVar -> LExpr TVar -> Unificator ()
algorithmW m (Var v i) =
  do
    i'  <- lift . lift $ case M.lookup v m of
      Nothing -> Left $ "Free variable '" ++ v ++ "'."
      Just i' -> return i'
    ctx <- get
    t'  <- case M.lookup i' ctx of
      Nothing -> return $ TVar i'
      Just t -> 
        case t of
          Mono t'    -> return t'
          Poly vs t' -> lift $ foldr rename (return t') vs
    addAssumption i t'
algorithmW m (Lam x e i) =
  do
    ix <- lift nextID
    addAssumption i $ TVar ix :-> TVar (typeOf e)
    algorithmW (M.insert x ix m) e
algorithmW m (Let x e e' i) =
  do
    ix <- lift nextID
    let m' = M.insert x ix m
    addAssumption ix $ TVar (typeOf e)
    algorithmW m' e
    polymorphize m ix
    addAssumption i $ TVar (typeOf e')
    algorithmW m' e'
algorithmW m (App e e' i) =
  do
    addAssumption (typeOf e) $ TVar (typeOf e') :-> TVar i
    algorithmW m e
    algorithmW m e'

addAssumption :: TVar -> MonoType -> Unificator ()
addAssumption v a = get >>= (lift . lift . unifyEq v a) >>= put

polymorphize :: Map VarName TVar -> TVar -> Unificator ()
polymorphize m ix = do
  ctx <- get >>= lift . lift . inlineTypes
  put $ M.insert ix (poly ctx) ctx
  where
    poly ctx | null vs   = Mono t
             | otherwise = Poly vs t
      where
        used i = maybe (S.singleton i) usedVars $ M.lookup i ctx
        vs     = S.toList $ used ix \\ outer
        outer  = M.fold (S.union . used) S.empty m
        Mono t = fromMaybe (Mono $ TVar ix) $ M.lookup ix ctx

rename :: TVar -> VarState MonoType -> VarState MonoType
rename v mt = nextID >>= \v' -> rename' v' <$> mt
  where rename' v' = replaceTVar $ \w -> TVar $ if v == w then v' else w

unifyEq :: TVar -> MonoType -> Context -> Either Error Context
unifyEq v0 (TVar w) ctx0 | v0 == w = return ctx0
unifyEq v0 a0       ctx0 = trace ("unify: T" ++ show v0 ++ " = " ++ show a0) $ go ctx0 $ Q.fromList [(v0,a0)]
  where
    go :: Context -> Q.Queue (TVar, MonoType) -> Either Error Context
    go ctx q = trace ("  queue: " ++ show q) $ case Q.dequeue q of
      Nothing          -> return ctx
      Just ((v,a), q') -> do
        occursCheck v a
        case M.lookup v ctx of
          Nothing -> go (M.insert v (Mono a) ctx) q'
          Just t' -> case t' of
            Poly _ _ -> Left $ "Unexpected polymorphism: " ++ show t'
            Mono b   -> do
              es <- unifyTypes a b
              go ctx $ Q.enqueueAll es q'
    occursCheck :: TVar -> MonoType -> Either Error ()
    occursCheck v t = when (v `occursIn` t) $
      Left $ "Occurs check: " ++ show v ++ " = " ++ show (Mono t)

unifyTypes :: MonoType -> MonoType -> Either Error [Equation]
unifyTypes a0 b0 =
    (\res -> trace ("unifyTypes " ++ show (a0,b0) ++ " = " ++ show res) res) <$>
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

    failUni a b = Left $ "Types '" ++ show a ++ "' and '"
                                   ++ show b ++ "' cannot be unified."

inlineTypes :: Context -> Either Error Context
inlineTypes ctx = flip execStateT M.empty $ mapM_ (dfs S.empty) $ M.keys ctx
  where
    dfs open v = do
      closed <- get
      case M.lookup v closed of
        Just t' -> return t'
        Nothing -> do
          when (v `S.member` open) $
            lift . Left $ "Occurs check: " ++ show (TVar v)
          case M.lookup v ctx of
            Nothing -> return $ Mono (TVar v)
            Just pt -> do
              t' <- case pt of
                      Mono    t -> Mono    <$> inline t
                      Poly vs t -> Poly vs <$> inline t
              modify $ M.insert v t'
              return t'
      where
        inline :: MonoType -> StateT Context (Either Error) MonoType
        inline = tFoldA $ \w -> do
          t <- dfs open' w
          case t of
            Mono t' -> return t'
            _       -> lift . Left $ "Unexpected polymorphism: " ++ show t
        open' = S.insert v open

nextID :: VarState TVar
nextID = get >>= \k -> (put $! k+1) >> return k

{------------------------------------------------------------------------------}

setIDs :: LExpr a -> VarState (LExpr TVar)
setIDs (Var v _)      =
  Var v <$> nextID
setIDs (Lam x e _)    = nextID >>= \t ->
  Lam x <$> setIDs e <*> pure t
setIDs (Let x e e' _) = nextID >>= \t ->
  Let x <$> setIDs e <*> setIDs e' <*> pure t
setIDs (App e e' _)   = nextID >>= \t ->
  App   <$> setIDs e <*> setIDs e' <*> pure t

adoptVars :: Map VarName PolyType -> VarState (Map VarName TVar, Context)
adoptVars = M.foldrWithKey' insert $ return (M.empty, M.empty)
  where
    insert n t mr = do
      k <- nextID
      (m', eq) <- mr
      t' <- evalStateT (adopt t) M.empty
      return (M.insert n k m', M.insert k t' eq)

    adopt :: PolyType -> StateT (Map TVar MonoType) VarState PolyType
    adopt (Mono    t) = Mono <$> adopt' t
    adopt (Poly vs t) = Poly <$> mapM newVar vs <*> adopt' t

    adopt' :: MonoType -> StateT (Map TVar MonoType) VarState MonoType
    adopt' = tFoldA $ \v -> get >>= (maybe (TVar <$> newVar v) return . M.lookup v)

    newVar :: TVar -> StateT (Map TVar MonoType) VarState TVar
    newVar v = lift nextID >>= \v' -> (modify . M.insert v $ TVar v') >> return v'

