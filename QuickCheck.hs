{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Function ( Fun ( Fun ) )
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
    let (targs, _) = uncurryType ttype
    let ar = length targs
    xs <- replicateM ar (newName "x")

    let pats = zipWith mkpat targs xs
        args = zipWith mkvar targs xs
    pure $ LamE pats (VarE '(===) `AppE` (apply tname args) `AppE` (apply sname args))

  where
    mkpat :: Type -> Name -> Pat
    mkpat ft@(AppT (AppT ArrowT _) _) x = ConP 'Fun [WildP, VarP x]
    mkpat _ x = VarP x

    mkvar :: Type -> Name -> Exp
    mkvar ft@(AppT (AppT ArrowT _) _) x = VarE uc `AppE` VarE x
      where
        (ta, _) = uncurryType ft
        uc = mkName ("curry" ++ show (length ta))
    mkvar _ x = VarE x

apply :: Name -> [Exp] -> Exp
apply name exs = foldl AppE (VarE name) exs

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
