{-# LANGUAGE FlexibleContexts #-}

module Transpile where

import Rust.AST as R
import Target as T

import Control.Monad.Writer
import Control.Monad.State

import qualified Data.Map as M

data TranspileState = TranspileState {
  _nameCount :: Int
, _regMap    :: M.Map RegVar Name
}

initTranspileState :: TranspileState
initTranspileState = TranspileState 0 M.empty

type Transpile a = WriterT (State TranspileState (a, [Statement]))

runTranspile :: T.Expr -> (Name, [Statement])
runTranspile e = fst $ runState (runWriterT (transpile e)) initTranspileState

transpile (EInt i r)   = mkIntBox i r
transpile (EVar x)     = return x
transpile (EApp e1 e2) = do
    v1 <- transpile e1
    v2 <- transpile e2
    app v1 v2

transpile (EAbs x e r) = mkClosureBox x e r

transpile (ENew r e)   = do
    ret <- transpile e
    dropBox (RegVar r)
    return ret


mkIntBox i r = do
    x <- freshName
    mark x r
    emit $ Let Imm (x, Just (PrimType IntTy))
                   (Nothing, TmExpr (TmAtom (TmLit (IntLit i))))
    return x

app v1 v2 = do
    x <- freshName
    emit $ Let Imm (x, Nothing)
                   (Nothing, TmExpr (TmCall v1 [TmExpr (TmAtom (NamedVar v2))]))
    return x

mkClosureBox x e r = do
    let (ret, stmts) = runTranspile e
    let term = TmClosure [(x, Nothing)] (Body stmts (Just (TmExpr (TmAtom (NamedVar ret)))))
    f <- freshName
    mark f r
    emit $ Let Imm (f, Nothing) (Nothing, TmExpr (TmAtom term))
    return f

dropBox r = do
    x <- query r
    emit $ SCall "drop" [TmExpr (TmAtom (NamedVar x))]

freshName = do
    s <- get
    let i = _nameCount s
    put s { _nameCount = i + 1 }
    return $ "x" ++ show i

emit x = tell [x]

query r = do
    m <- _regMap <$> get
    case M.lookup r m of
        Just x  -> return x
        Nothing -> error $ "can't find " ++ show r

mark x r = do
    m <- _regMap <$> get
    s <- get
    put s { _regMap = M.insert r x m }
