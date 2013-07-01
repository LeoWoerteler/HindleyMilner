module Main where

import LExpr         ( LExpr, parseLExpr, lam, var, let', ($:) )
import Data.Maybe    ( fromJust )
import Control.Monad ( when )
import Data.List     ( isPrefixOf )
import Data.IORef
import qualified Data.Map as M ( Map, empty, singleton, fromList )

import HindleyMilner

a, b, c :: MonoType
[a, b, c] = map TVar [1..3]

tMap, tComp :: PolyType
tMap  = Poly [1, 2] $ (a :-> b) :-> TCons "List" [a] :-> TCons "List" [b]
tComp = Poly [1, 2, 3] $ (b :-> c) :-> (a :-> b) :-> a :-> c

lBar, lFlip, lComp, lFooBar, lPart, lTwice :: LExpr ()
lBar = lam "f" . lam "x" . lam "y" $ var "f" $: var "x" $: (var "f" $: var "y" $: var "x")
lFlip = lam "f" . lam "x" . lam "y" $ var "f" $: var "y" $: var "x"
lComp = lam "f" . lam "g" . lam "x" $ var "f" $: (var "g" $: var "x")
-- \f g x . let h = \y . y x in (h f (h g))
lFooBar = lam "f" . lam "g" . lam "x" $
  let' "h" (lam "y" $ var "y" $: var "x") $ var "h" $: var "f" $: (var "h" $: var "g")
-- \x y -> let c = \z -> x in c (c y)
lPart = lam "x" . lam "y" $ let' "c" (lam "z" $ var "x") $ var "c" $: (var "c" $: var "y")
lTwice = lam "f" . lam "x" $ var "f" $: (var "f" $: var "x")

eComp, eOwl, eFoo, eTwice :: LExpr PolyType
-- \f g x -> f (g x)
eComp = testHM' [] lComp
-- ((.).(.))
eOwl  = testHM' [("(.)", typeOf eComp)] $ var "(.)" $: var "(.)" $: var "(.)"
-- \x . map (\y . x)
eFoo  = testHM' [("map", tMap)] $ lam "x" $ var "map" $: lam "y" (var "x")
-- \f x . f (f x)
eTwice  = testHM' [] lTwice
-- map . map

eMapMap = testHM [("(.)", tComp), ("map", tMap)] $
  (var "(.)" $: var "map") $: var "map"

testHM :: [(String, PolyType)] -> LExpr a -> Either String (LExpr PolyType)
testHM = hindleyMilner . M.fromList

testHM' :: [(String, PolyType)] -> LExpr a -> LExpr PolyType
testHM' bs e = either error id $ testHM bs e

main::IO()
main = newIORef M.empty >>= mainLoop

mainLoop :: IORef (M.Map String PolyType) -> IO ()
mainLoop ref = loop
  where
    loop = do
      line <- putStr "> " >> getLine
      case line of
        ":exit" -> putStrLn "Goodbye :-)."
        _       -> do
          processLine line
          loop
    processLine l = case parseLExpr l of
      Left err   -> putStrLn $ "Parsing error:\n\t" ++ err
      Right expr -> do
        putStrLn $ "Expression: " ++ show expr
        putStrLn $ case testHM [] expr of
          Left  err   -> err
          Right expr' -> "Type: " ++ show (typeOf expr')
