module Source where

import Common

data Expr = EInt Int
          | EVar Name
          | EApp Expr Expr
          | EAbs Name Expr
          deriving (Show, Eq)
