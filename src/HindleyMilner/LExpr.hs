
{-# LANGUAGE DeriveFunctor #-}

module HindleyMilner.LExpr (
  LExpr(..),
  VarName,
  typeOf,
  lFold,
  var, lam, let', ($:),
  parseLExpr
) where

import Control.Applicative ( Applicative(..), (<$>), (<|>) )
import Text.Parsec.String  ( Parser )
import Text.Parsec         ( string, letter, alphaNum, char, space, spaces, eof,
                             many, many1, chainl1, try, unexpected, runParser )

type VarName = String

data LExpr t
  = Var VarName                     t
  | Lam VarName (LExpr t)           t
  | App         (LExpr t) (LExpr t) t
  | Let VarName (LExpr t) (LExpr t) t
  deriving Functor

var :: String -> LExpr ()
var x = Var x ()

lam :: String -> LExpr () -> LExpr ()
lam v e = Lam v e ()

let' :: String -> LExpr () -> LExpr () -> LExpr ()
let' v e e' = Let v e e' ()

($:) :: LExpr () -> LExpr () -> LExpr ()
e $: e' = App e e' ()

typeOf :: LExpr a -> a
typeOf = lFold (\_ t -> t) c2 c2 (\_ _ _ t -> t)
  where c2 _ _ t = t

lFold :: (String -> t -> a)
      -> (String -> a -> t -> a)
      -> (a -> a -> t -> a)
      -> (String -> a -> a -> t -> a)
      -> LExpr t
      -> a
lFold vr lm ap lt = go
  where
    go (Var v t)      = vr v t
    go (Lam x e t)    = lm x (go e) t
    go (App e e' t)   = ap (go e) (go e') t
    go (Let x e e' t) = lt x (go e) (go e') t

parseLExpr :: String -> Either String (LExpr ())
parseLExpr str = case runParser (spaces *> (parseExpr <* eof)) () "" str of
                     Left  err  -> Left $ show err
                     Right expr -> Right expr

parseExpr :: Parser (LExpr ())
parseExpr  =  chainl1 (parseSingle <* spaces) (pure ($:))
          <|> try (string "let" *> space) *> spaces *> parseLet
          <|> char '\\' *> spaces *> parseLambda

parseSingle :: Parser (LExpr ())
parseSingle  =  char '(' *> spaces *> parseExpr <* char ')' <* spaces
            <|> var <$> try parseVar

parseVar :: Parser String
parseVar = (:) <$> letter <*> many alphaNum <* spaces >>= noReserved
  where
    noReserved v | v `elem` ["let", "in"] = unexpected v
                 | otherwise              = return v

parseLambda :: Parser (LExpr ())
parseLambda = lambda <$> many1 parseVar <* char '.' <* spaces
                     <*> parseExpr
  where lambda vs e = foldr lam e vs

parseLet :: Parser (LExpr ())
parseLet = let' <$> parseVar  <* char '='    <* spaces
                <*> parseExpr <* string "in" <* spaces
                <*> parseExpr

instance Show (LExpr t) where
  showsPrec _ (Var x _)      = showString x
  showsPrec p (App e e' _)   = showParen (p > 1) $
    showsPrec 1 e . (' ':) . showsPrec 2 e'
  showsPrec p (Let x e e' _) = showParen (p > 0) $
    ("let " ++) . (x ++) . (" = " ++) . shows e . (" in " ++) . shows e'
  showsPrec p (Lam x e _)    = showParen (p > 0) $ go (('\\':x) ++) e
    where
      go s l = case l of
        Lam x' e' _ -> go (s . ((' ':x') ++)) e'
        _           -> s . showString " . " . shows l
