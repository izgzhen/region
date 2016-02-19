module Infer where

import qualified Source as S
import qualified Target as T
import Target (RegVar(..), frv)

import Control.Monad.State
import Control.Monad.Except

data InferState = InferState {
    _regVarCount :: Int
}

initInferState :: InferState
initInferState = InferState 0

type Infer = ExceptT String (State InferState)

runInfer :: S.Expr -> Either String T.Expr
runInfer e = evalState (runExceptT (infer' e)) initInferState

infer' :: S.Expr -> Infer T.Expr
infer' sexpr = do
    texpr <- infer sexpr
    let regvars = frv texpr
    return (foldr T.ENew texpr regvars)

infer :: S.Expr -> Infer T.Expr
infer (S.EInt i) = T.EInt i <$> freshRegVar
infer (S.EVar x) = return $ T.EVar x
infer (S.EApp e1 e2) = T.EApp <$> infer e1 <*> infer e2
infer (S.EAbs x e) = T.EAbs x <$> infer e <*> freshRegVar

freshRegVar :: Infer RegVar
freshRegVar = do
    s <- get
    let i = _regVarCount s
    put s { _regVarCount = i + 1 }
    return $ RegVar ("ρ" ++ show i)



