{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Function ( Fun ( Fun ) )
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns
import Curry

sprop :: String -> String -> Q Exp
sprop teacher student = (,) <$> lookupValueName teacher <*> lookupValueName student >>= \case
    (Just tname, Just sname) -> prop tname sname
    (Nothing, Nothing) -> fail $ "sprop: Could not find " ++ teacher ++ " and " ++ student
    (Nothing, _)       -> fail $ "sprop: Could not find " ++ teacher
    (_, Nothing)       -> fail $ "sprop: Could not find " ++ student

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
    let (targs, _) = uncurryType ttype
    let ar = length targs
    xs <- replicateM ar (newName "x")

    pats <- zipWithM mkpat targs xs
    args <- zipWithM mkvar targs xs
    pure $ LamE pats (VarE '(===) `AppE` (apply tname args) `AppE` (apply sname args))

  where
    -- | construct a pattern from its type and variable name (@x@)
    -- * for function types, it constructs @Fun _ x@
    -- * if the type is not Show-able, wraps the pattern in 'Blind'
    -- * otherwise, it constructs @x@
    mkpat :: Type -> Name -> Q Pat
    mkpat t x = do
        arb <- hasArbitrary t
        sh <- hasShow t
        when (not arb) . fail $ "testFun': no instance of arbitrary for " ++ show (ppr t)
        if sh
        then pure base
        else do
            reportWarning $ "testFun': no instance of show for " ++ show (ppr t) ++ ", using Blind"
            pure $ ConP 'Blind [base]
      where
        base | isFunctionType t = ConP 'Fun [WildP, VarP x]
             | otherwise        = VarP x

    mkvar :: Type -> Name -> Q Exp
    mkvar t x = pure base
      where
        base | isFunctionType t = VarE uc `AppE` VarE x
             | otherwise = VarE x

        (ta, _) = uncurryType t
        uc = mkName ("curry" ++ show (length ta))

-- | Is the top-level type constructor a fully applied (->)?
isFunctionType :: Type -> Bool
isFunctionType (AppT (AppT ArrowT _) _) = True
isFunctionType _                        = False

hasShow :: Type -> Q Bool
hasShow t = t `hasInstance` ''Show

hasArbitrary :: Type -> Q Bool
hasArbitrary t = t `hasInstance` ''Arbitrary

apply :: Name -> [Exp] -> Exp
apply name exs = foldl AppE (VarE name) exs

hasInstance :: Type -> Name -> Q Bool
hasInstance t cls = (== 1) . length <$> reifyInstances cls [t]

-- | @$('showQ' $ arity <$> [t| forall a . a -> (a -> a) -> (Int -> Int) |])@
arity :: Type -> Int
arity t = length (fst (uncurryType t))

uncurryType :: Type -> ([Type], Type)
uncurryType t0 = let t = unct t0 in (init t, last t)
  where
    unct (AppT (AppT ArrowT t1) t2) = t1 : unct t2
    unct (ForallT tyvs cxt ty) = unct ty
    unct x = [x]

$(genCurries 62)
