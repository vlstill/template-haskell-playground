{-# LANGUAGE TemplateHaskell #-}

-- source: https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial

module Functor where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( putQ, getQ, BangType )

data Deriving = Deriving { tyCon :: Name, tyVar :: Name }
 
deriveFunctor :: Name -> Q [Dec]
deriveFunctor ty = do
    (TyConI tyCon) <- reify ty
    (tyConName, tyVars, cs) <- case tyCon of
        DataD _ nm tyVars _ cs _   -> return (nm, tyVars, cs)
        NewtypeD _ nm tyVars _ c _ -> return (nm, tyVars, [c])
        _ -> fail "deriveFunctor: tyCon may not be a type synonym."
 
    let (KindedTV tyVar StarT) = last tyVars
        instanceType           = conT ''Functor `appT`
                                 (foldl apply (conT tyConName) (init tyVars))
 
    putQ $ Deriving tyConName tyVar
    sequence [instanceD (return []) instanceType [genFmap cs]]
  where
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)


genFmap :: [Con] -> Q Dec
genFmap cs = funD 'fmap (map genFmapClause cs)
 
genFmapClause :: Con -> Q Clause
genFmapClause c@(NormalC name fieldTypes) = do
    f          <- newName "f"
    fieldNames <- replicateM (length fieldTypes) (newName "x")
 
    let pats = varP f:[conP name (map varP fieldNames)]
        body = normalB $ appsE $
                conE name : map (newField f) (zip fieldNames fieldTypes)

    clause pats body []
 
newField :: Name -> (Name, BangType) -> Q Exp
newField f (x, (_, fieldType))
  = do Just (Deriving typeCon typeVar) <- getQ
       case fieldType of
         VarT typeVar' | typeVar' == typeVar ->
           [| $(varE f) $(varE x) |]
         ty `AppT` VarT typeVar' |
           leftmost ty == (ConT typeCon) && typeVar' == typeVar ->
             [| fmap $(varE f) $(varE x) |]
         _ -> [| $(varE x) |]
 
leftmost :: Type -> Type
leftmost (AppT ty1 _) = leftmost ty1
leftmost ty           = ty
