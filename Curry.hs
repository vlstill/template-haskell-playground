{-# LANGUAGE TemplateHaskell #-}

module Curry where

import Control.Monad
import Language.Haskell.TH

-- | For each n >= 0 build a generalization of curry function of given arity
curryN :: Int -> Q Exp
curryN n = do
    -- get names for function arguments
    f <- newName "f"
    xs <- replicateM n (newName "x")
    let args = map VarP (f:xs) -- build function patterns
        ntup = TupE (map VarE xs) -- build tuple expression
    pure $ LamE args (AppE (VarE f) ntup) -- generate the actual curryN as a lambda

-- | For each n >= 0 build a generalization of uncurry function of given arity
uncurryN :: Int -> Q Exp
uncurryN n = do
    f <- newName "f"
    xs <- replicateM n (newName "x")
    let args = [VarP f, TupP (map VarP xs)] -- the two arguments of the lambda
        vars = map VarE xs
           -- note the use of foldl to build currified application
    pure $ LamE args (foldl AppE (VarE f) vars)
