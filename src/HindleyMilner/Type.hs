
module HindleyMilner.Type (
  MonoType(..),
  PolyType(..),
  occursIn,
  usedVars,
  replaceTVar,
  tMapA,
  parsePolyType
) where

import Data.Char           ( isLower, isUpper )
import Data.Map            ( Map )
import Data.Set            ( Set, (\\) )
import Control.Applicative ( Applicative(..), (<$>), (<|>), liftA2 )
import Text.Parsec.String  ( GenParser )
import Text.Parsec         ( getState, setState, string, alphaNum, char, spaces,
                             eof, many, chainr1, satisfy, runParser )

import qualified Data.Map as M ( insert, empty, lookup, elems )
import qualified Data.Set as S ( singleton, fromList, union, unions )

type Parser = GenParser Char (Int, Map String Int)

data MonoType
  = TVar Int                -- type variable
  | MonoType :-> MonoType   -- the function arrow
  | TCons String [MonoType] -- type constructor, e.g. `[a]`, `Bool`, `(a,b,c)`

infixr 5 :->

data PolyType = Poly [Int] MonoType

tFold :: (Int -> a) -> (a -> a -> a) -> (String -> [a] -> a) -> MonoType -> a
tFold var (~>) cons = go
  where
    go (TVar x)     = var x
    go (a :-> b)    = go a ~> go b
    go (TCons n ts) = cons n $ map go ts

occursIn :: Int -> MonoType -> Bool
occursIn v = tFold (v ==) (||) (const or)

usedVars :: PolyType -> Set Int
usedVars (Poly vs t) = used t \\ S.fromList vs
  where used = tFold S.singleton S.union $ const S.unions

replaceTVar :: (Int -> MonoType) -> MonoType -> MonoType
replaceTVar f = tFold f (:->) TCons

tMapA :: Applicative f => (Int -> f MonoType) -> MonoType -> f MonoType
tMapA f = tFold f (liftA2 (:->)) $ \n -> fmap (TCons n) . sequenceA
  where sequenceA = foldr (liftA2 (:)) (pure [])

instance Show MonoType where
  showsPrec = showsMonoType M.empty

instance Show PolyType where
  showsPrec = showsPolyType M.empty $ varNames [""]

varNames :: [String] -> [String]
varNames = tail . go
  where go xs = xs ++ go [ c:x | c <- ['a'..'z'], x <- xs ]

showsMonoType :: Map Int String -> Int -> MonoType -> ShowS
showsMonoType m _ (TVar i)     =
  maybe (showChar 'T' . shows i) showString $ M.lookup i m
showsMonoType m p (a :-> b)    = showParen (p > 0) $
  showsMonoType m 1 a . showString " -> " . showsMonoType m 0 b
showsMonoType m p (TCons n ts) = showParen (p > 1 && (not . null) ts) $
  foldl (\s t -> s . (' ':) . showsMonoType m 2 t) (showString n) ts

showsPolyType :: Map Int String -> [String] -> Int -> PolyType -> ShowS
showsPolyType _ _  p (Poly [] t) = showsMonoType M.empty p t
showsPolyType m ns p (Poly vs t) = showParen (p > 0) $
  showString "forall " . args . showString ". " . showsMonoType m'' 0 t
  where
    (m'', _, args) = foldl nameVars (m, ns, id) vs
    nameVars (m', ns', a) x = (M.insert x n m', ns'', a . (n ++) . (' ':))
      where (n:ns'') = ns'

parsePolyType :: String -> Either String PolyType
parsePolyType str = case runParser parse (0, M.empty) "" str of
  Left  err  -> Left $ show err
  Right expr -> Right  expr
  where
    parse = do
      e <- parseMType <* eof
      (_, m) <- getState
      return $ Poly (M.elems m) e

parseMType  :: Parser MonoType
parseMType   =  chainr1 parseSingle (string "->" *> spaces *> pure (:->))

parseSingle :: Parser MonoType
parseSingle  =  TCons <$> parseName isUpper <* spaces <*> many parseSimple
            <|> parseSimple

parseSimple :: Parser MonoType
parseSimple  =  TVar  <$> parseTVar
            <|> TCons <$> parseName isUpper <*> pure [] <* spaces
            <|> char '(' *> spaces *> parseMType <* char ')' <* spaces

parseName :: (Char -> Bool) -> Parser String
parseName p = (:) <$> satisfy p <*> many alphaNum

parseTVar :: Parser Int
parseTVar = do
  v <- parseName isLower <* spaces
  (i, m) <- getState
  case M.lookup v m of
    Just j  -> return j
    Nothing -> do
      setState (i + 1, M.insert v i m)
      return i
