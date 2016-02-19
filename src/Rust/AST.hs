{-# LANGUAGE TypeOperators #-}

module Rust.AST (
  Program(..)
, Decl(..)
, Expr(..)
, Term(..)
, Atom(..)
, BiOp(..)
, Binding(..)
, Statement(..)
, Body(..)
, Modifier(..)
, Mutability(..)
, module Rust.Shared
, varExpr
, biOpOf
) where

{-
    The AST defined here should be syntactically correct subset of Rust Language;
    The compiled code should also guarantee the semantical correctness
-}

import Rust.Shared

data Decl = StructDecl Name [(Name, Ty)]
          | EnumDecl   Name [(Name, [Ty])]
          | FuncDecl   Name [(Name, Ty)] (Maybe Ty) Body
          deriving (Show, Eq)

data Expr = InfixExpr BiOp Expr Expr
          | TmExpr Term
          deriving (Show, Eq)

varExpr = TmExpr . TmAtom . NamedVar

data BiOp = Plus deriving (Show, Eq)

biOpOf :: Name -> BiOp
biOpOf name = case name of
    "+" -> Plus
    x   -> error $ "no such biOp: " ++ x

data Term = TmCall Name [Expr]
          | TmAtom Atom
          deriving (Show, Eq)

data Atom = NamedVar Name
          | TmLit Literal
          | TmTuple [Expr]
          -- | TmVec [Expr] -- XXX: Rust used Macro to implement a special syntax vec![], ARE THEY EQUIVALENT? 
          | TmClosure [Binding] Body -- XXX: `move`
          deriving (Show, Eq)

type Binding = (Name, Maybe Ty)

data Body = Body [Statement] (Maybe Expr) -- It might end with an expression
          deriving (Show, Eq)

data Statement = Let Mutability Binding (Maybe Modifier, Expr)
               | IfThenElse Expr [Statement] (Maybe [Statement])
               | While Expr [Statement]
               | SCall Name [Expr]
               | Return Name
               | (Int, Name) := (Maybe Modifier, Expr) -- Int: Depth of dereferencing
               deriving (Show, Eq)


data Modifier   = RefMut | Ref deriving (Show, Eq)
data Mutability = Mut    | Imm deriving (Show, Eq)

data Program = Program [Decl] deriving (Show, Eq)
