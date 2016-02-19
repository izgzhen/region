module Target where

import Common
import Data.Set

data Expr = EInt Int RegVar
          | EVar Name
          | EApp Expr Expr
          | EAbs Name Expr RegVar
          | ENew Name Expr
          deriving (Eq)

instance Show Expr where
    show (EInt i reg)   = show i ++ "@" ++ show reg
    show (EVar x)       = x
    show (EApp e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (EAbs x e reg) = "(λ" ++ x ++ "." ++ show e ++ ")@" ++ show reg
    show (ENew x e)     = "(new " ++ x ++ "." ++ show e ++ ")"

newtype RegVar = RegVar { unRegVar :: Name } deriving (Eq, Ord)

instance Show RegVar where
    show (RegVar x) = x

frv :: Expr -> Set Name
frv (EInt _ r)   = singleton $ unRegVar r
frv (EVar _)     = empty
frv (EApp e1 e2) = frv e1 `union` frv e2
frv (EAbs _ e r) = frv e `union` singleton (unRegVar r)
frv (ENew r e)   = delete r (frv e)
