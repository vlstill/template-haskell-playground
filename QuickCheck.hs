{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module QuickCheck where

import Test.QuickCheck
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Curry

-- | $(prop 'a 'b) :: Property
-- >>> quickCheck $(prop 'drop 'drop)
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $(prop 'drop 'take)
-- *** Failed! Falsifiable (after 3 tests):                  
-- 0
-- [()]
-- [()] /= []
prop :: Name -> Name -> Q Exp
prop teacher student = (,) <$> reify teacher <*> reify student >>= \case
    (VarI tnam ttype _, VarI snam stype _) -> testFun tnam ttype snam stype
    (ClassOpI tnam ttype _, ClassOpI snam stype _) -> testFun tnam ttype snam stype
    (t, s) -> fail $ "prop: Invarid arguments for prop:\n        " ++ show (ppr t) ++ "\n        " ++ show (ppr s)

testFun :: Name -> Type -> Name -> Type -> Q Exp
testFun tname ttype sname stype
    | arity ttype /= arity stype = fail "testFun: types not equal"
    | otherwise = testFun' tname ttype sname stype

testFun' :: Name -> Type -> Name -> Type -> Q Exp
testFun' tname ttype sname stype = do
    let ar = arity ttype
    xs <- replicateM ar (newName "x")
    let pats = map VarP xs
        args = map VarE xs
    pure $ LamE pats (VarE '(===) `AppE` (apply tname args) `AppE` (apply sname args))

apply :: Name -> [Exp] -> Exp
apply name exs = foldl AppE (VarE name) exs

-- | @$('showQ' $ arity <$> [t| forall a . a -> (a -> a) -> (Int -> Int) |])@
arity :: Type -> Int
arity (ForallT _ _ t) = arity t
arity (AppT (AppT ArrowT _) t) = 1 + arity t
arity _ = 0
