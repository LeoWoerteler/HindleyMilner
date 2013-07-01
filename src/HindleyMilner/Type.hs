
module HindleyMilner.Type (
  MonoType(..),
  PolyType(..),
  occursIn,
  usedVars,
  replaceTVar,
  tFoldA
) where

import Data.List              ( foldl' )
import Data.Map               ( Map )
import Data.Set               ( Set, (\\) )
import Control.Applicative    ( (<$>), Applicative(..) )
import Control.Monad.State    ( State, evalState, get, put )
import Control.Monad.Identity ( Identity(..) )

import qualified Data.Map as M ( insert, empty, lookup )
import qualified Data.Set as S ( empty, singleton, fromList, union )

data MonoType
  = TVar Int                -- type variable
  | MonoType :-> MonoType   -- the function arrow
  | TCons String [MonoType] -- type constructor, e.g. `[a]`, `Bool`, `(a,b,c)`

infixr 5 :->

data PolyType
  = Mono MonoType
  | Poly [Int] MonoType

occursIn :: Int -> MonoType -> Bool
v `occursIn` TVar w     = v == w
v `occursIn` (a :-> b)  = v `occursIn` a || v `occursIn` b
v `occursIn` TCons _ ts = (v `occursIn`) `any` ts

usedVars :: PolyType -> Set Int
usedVars p = case p of
  Mono    t -> used t
  Poly vs t -> used t \\ S.fromList vs
  where
    used (TVar x)     = S.singleton x
    used (a :-> b)    = used a `S.union` used b
    used (TCons _ ts) = foldr (S.union . used) S.empty ts

replaceTVar :: (Int -> MonoType) -> MonoType -> MonoType
replaceTVar f = runIdentity . tFoldA (Identity . f)

tFoldA :: Applicative f => (Int -> f MonoType) -> MonoType -> f MonoType
tFoldA f = go
  where
    go (TVar v)     = f v
    go (a :-> b)    = (:->) <$> go a <*> go b
    go (TCons n ts) = TCons n <$>
      foldr (\t r -> (:) <$> go t <*> r) (pure []) ts

instance Show MonoType where
  showsPrec _ (TVar i)  = ('T':) . shows i
  showsPrec p (a :-> b) = showParen (p > 0) $
    showsPrec 1 a . (" -> " ++) . shows b
  showsPrec p (TCons n ts) = showParen (p > 1 && (not . null) ts) $
    showString n . foldr (\x r -> (' ':) . showsPrec 2 x . r) id ts

showsMonoType :: Int -> MonoType -> State (Map Int String, [String]) ShowS
showsMonoType _ (TVar i) = do
  (m, vs) <- get
  case M.lookup i m of
    Just v  -> return $ showString v
    Nothing -> do
      let (v:vs') = vs
      put (M.insert i v m, vs')
      return $ showString v
showsMonoType p (a :-> b) = do
  sa <- showsMonoType 1 a
  sb <- showsMonoType 0 b
  return $ showParen (p > 0) $ sa . showString " -> " . sb
showsMonoType p (TCons n ts) = do
  ts' <- foldr showsMono (return id) ts
  return . showParen (p > 1 && (not . null) ts) $ showString n . (' ':) . ts'
  where showsMono t m = showsMonoType 2 t >>= \st -> (.) ((' ':) . st) <$> m

varNames :: [String] -> [String]
varNames = tail . go
  where go xs = xs ++ [ c:x | c <- ['a'..'z'], x <- xs ]

showsPolyType :: Int -> PolyType -> State (Map Int String, [String]) ShowS
showsPolyType p (Mono t)    = showsMonoType p t
showsPolyType p (Poly vs t) = do
  args <- foldl' nameVars (return $ showString "forall ") vs
  (\t' -> showParen (p > 0) $ args . showString ". " . t') <$> showsMonoType 0 t
  where
    nameVars ma t' = do
      args' <- ma
      (m,v:vs') <- get
      put (M.insert t' v m, vs')
      return $ args' . showString v . (' ':)

instance Show PolyType where
  showsPrec p t = evalState (showsPolyType p t) (M.empty, varNames [""])

  

