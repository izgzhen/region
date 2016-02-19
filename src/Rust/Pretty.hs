module Rust.Pretty (
  printProgram
, pStatement
) where

import Text.PrettyPrint.HughesPJ
import Rust.AST

endLine :: Doc -> Doc
endLine doc = doc <> char '\n'

startLine :: Doc -> Doc
startLine doc = char '\n' <> doc

unlinesCat :: Int -> [Doc] -> Doc
unlinesCat i = cat . map
                    ((text (take i $ repeat ' ') <>)
                    . endLine)

printProgram :: Program -> String
printProgram (Program decls) = render . unlinesCat 0 $ map printDecl decls

printDecl :: Decl -> Doc
printDecl (StructDecl name fields) =
        text "struct"
    <+> pName name
    <+> braces (startLine
              $ unlinesCat 4
              $ map printField fields)
  where
    printField (name, ty) = pName name <+> colon <+> pTy ty <> comma

printDecl (EnumDecl name cases) =
        text "enum"
    <+> pName name
    <+> braces (startLine
              $ unlinesCat 4
              $ map printCase cases)
  where
    printCase (name, tys) = pName name <> parens (map pTy tys `sepWith` comma) <> comma


pTy :: Ty -> Doc
pTy (PrimType prim)  = pPrim prim
pTy (NamedType name) = pName name
pTy (BuiltInType builtin) = pBuiltIn builtin
pTy (ClosureType params maybeRet) = 
  text "Box<Fn" <> parens (map pTy params `sepWith` comma)
                <> ret <> text ">"
  where
    ret = case maybeRet of
            Nothing -> text ""
            Just ty -> text " ->" <+> pTy ty

pTy (BoxedTy ty) = text "Box<" <> pTy ty <> text ">"
pTy TyUnit    = text "()"

-- XXX: How to deal with these types can be a tricky problem
pBuiltIn :: BuiltInTy -> Doc
-- XXX: we need a f, s.t. f :: IO a -> a
pBuiltIn (TyIO x)  = pTy x
pBuiltIn (TyMut x) = text "&mut" <+> pTy x
pBuiltIn (TyBox x) = text "Box<" <> pTy x <> text ">"

pPrim :: PrimTy -> Doc
pPrim IntTy     = text "i32"
pPrim BoolTy    = text "bool"
pPrim StringTy  = text "&'static str"

pName :: Name -> Doc
pName = text

sepWith :: [Doc] -> Doc -> Doc
sepWith []     _ = text ""
sepWith [x]    _ = x
sepWith (x:xs) s = x <> s <> sepWith xs s

pBinding :: Binding -> Doc
pBinding (name, maybeTy) = pName name <+> typeNotation
    where
        typeNotation = case maybeTy of
            Nothing -> text ""
            Just ty -> colon <+> pTy ty

pStatement :: Statement -> Doc
pStatement (Let mut binding (maybeModifier, expr)) =
      text "let" <+> mutNotation <> pBinding binding <> text "="
                 <+> printModifier maybeModifier <+> pExpr expr <> text ";"
    where
        mutNotation  = case mut of
            Mut -> text "mut"
            Imm -> text ""

pStatement (IfThenElse condExpr stmts maybeAltStmts) =
    text "if" <+> pExpr condExpr
              <+> braces (startLine
                    $ unlinesCat 4
                    $ map pStatement stmts)
              <+> text "else"
              <+> (case maybeAltStmts of
                        Nothing -> text ""
                        Just ss -> braces (startLine
                                    $ unlinesCat 4
                                    $ map pStatement ss))

pStatement (While condExpr stmts) = 
    text "while" <+> pExpr condExpr
                 <+> braces (startLine
                    $ unlinesCat 4
                    $ map pStatement stmts)

pStatement (SCall f es) =
    pName f <> parens (map pExpr es `sepWith` comma) <> text ";"

pStatement ((depth, name) := (maybeModifier, expr)) = 
    deref <> pName name <+> text "=" <+> printModifier maybeModifier <+> pExpr expr <> text ";"
    where
        deref = text $ take depth (repeat '*')

pExpr :: Expr -> Doc
pExpr (InfixExpr biOp e1 e2) = parens $ pExpr e1 <+> pBiOp biOp <+> pExpr e2
pExpr (TmExpr term) = pTerm term

pTerm :: Term -> Doc
pTerm (TmCall name exprs) = pName name <> parens (map pExpr exprs `sepWith` comma)
pTerm (TmAtom atom) = pAtom atom

pAtom :: Atom -> Doc
pAtom (NamedVar name) = pName name
pAtom (TmLit lit)     = pLit lit
pAtom (TmTuple exprs) = parens (map pExpr exprs `sepWith` comma)
-- pAtom (TmVec exprs)   = text "vec![" <> map pExpr exprs `sepWith` comma <> text "]"
pAtom (TmClosure bindings body) =
    text "|" <> map pBinding bindings `sepWith` comma <> text "|"
             <+> pBody body

pBody :: Body -> Doc
pBody (Body stmts maybeRet) = 
    braces (startLine
          $ unlinesCat 4
          $ map pStatement stmts ++ ret)
    where
        ret = case maybeRet of
            Nothing -> []
            Just e  -> [pExpr e]
                    
pLit :: Literal -> Doc
pLit (IntLit i)      = text $ show i
pLit (BoolLit b)     = if b then text "true" else text "false"
pLit (StringLit str) = text $ show str -- XXX: need a way to escape

pBiOp :: BiOp -> Doc
pBiOp Plus = text "+"

printModifier :: Maybe Modifier -> Doc
printModifier maybeModifier = 
    case maybeModifier of
        Nothing -> text ""
        Just RefMut -> text "&mut"
        Just Ref    -> text "&"
