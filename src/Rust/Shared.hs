module Rust.Shared (
  Name(..)
, PrimTy(..)
, Ty(..)
, BuiltInTy(..)
, Literal(..)
) where

-- Namer

type Name = String

-- Type

data PrimTy = IntTy      -- i32
            | BoolTy     -- bool
            | StringTy   -- &'static str
            deriving (Show, Eq)

data Ty = PrimType PrimTy
        | NamedType Name
        | BuiltInType BuiltInTy
        | ClosureType [Ty] (Maybe Ty) -- parameters, optional return type
        | BoxedTy Ty
        | TyUnit
        deriving (Show, Eq)


data BuiltInTy = TyIO  Ty
               | TyMut Ty
               | TyBox Ty
               deriving (Show, Eq)

-- Term

data Literal = IntLit Int
             | BoolLit Bool
             | StringLit String
             deriving (Show, Eq)
