--- --------------------------------------------------------------------------
--- Pretty printing of FlatCurry.
---
--- This library provides pretty-printers for FlatCurry modules and all
--- substructures.
---
--- @author  Björn Peemöller
--- @version August 2013
--- --------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module FlatCurryPretty where

import Pretty

import FlatCurry
import FlatCurryGoodies

--- Pretty printing for terminals, i.e., with a maximum width of 80 characters.
pPrint :: Doc -> String
pPrint = pretty 80

--- pretty-print a FlatCurry module
ppProg :: Prog -> Doc
ppProg (Prog m is ts fs os)
  =   ppHeader    m ts fs
  <$> ppImports   is
  <$> ppOpDecls   os
  <$> ppTypeDecls ts
  <$> ppFuncDecls fs

--- pretty-print the module header
ppHeader :: String -> [TypeDecl] -> [FuncDecl] -> Doc
ppHeader m ts fs
  = indent (sep [text "module" <+> text m, ppExports ts fs, text "where"])
    <> line

--- pretty-print the export list
ppExports :: [TypeDecl] -> [FuncDecl] -> Doc
ppExports ts fs = tupled (map ppTypeExport ts ++ ppFuncExports fs)

--- pretty-print a type export
ppTypeExport :: TypeDecl -> Doc
ppTypeExport (Type    qn vis _ cs)
  | vis == Private      = empty
  | all isPublicCons cs = ppPrefixOp qn <+> text "(..)"
  | otherwise           = ppPrefixOp qn <+> tupled (ppConsExports cs)
    where isPublicCons (Cons _ _ v _) = v == Public
ppTypeExport (TypeSyn qn vis _ _ )
  | vis == Private = empty
  | otherwise      = ppPrefixOp qn

--- pretty-print the export list of constructors
ppConsExports :: [ConsDecl] -> [Doc]
ppConsExports cs = [ ppPrefixOp qn | Cons qn _ Public _ <- cs]

--- pretty-print the export list of functions
ppFuncExports :: [FuncDecl] -> [Doc]
ppFuncExports fs = [ ppPrefixOp qn | Func qn _ Public _ _ <- fs]

--- pretty-print a list of import statements
ppImports :: [String] -> Doc
ppImports []       = empty
ppImports is@(_:_) = vsep (map (indent . ppImport) is) <> line
  where ppImport m = text "import" <+> text m

--- pretty-print a list of operator fixity declarations
ppOpDecls :: [OpDecl] -> Doc
ppOpDecls []       = empty
ppOpDecls os@(_:_) = vsep (map (indent . ppOpDecl) os) <> line
  where ppOpDecl (Op qn fix n) = ppFixity fix <+> int n <+> ppInfixOp qn

--- pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

--- pretty-print a list of type declarations
ppTypeDecls :: [TypeDecl] -> Doc
ppTypeDecls []       = empty
ppTypeDecls ts@(_:_) = vsep (map (indent . ppTypeDecl) ts) <> line

--- pretty-print a type declaration
ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (Type    qn _ vs cs) = text "data" <+> ppQName qn
  <> hsep (empty : map ppTVarIndex vs) <$> ppConsDecls cs
ppTypeDecl (TypeSyn qn _ vs ty) = text "type" <+> ppQName qn
  <> hsep (empty : map ppTVarIndex vs) </> equals <+> ppTypeExpr 0 ty

--- pretty-print the constructor declarations
ppConsDecls :: [ConsDecl] -> Doc
ppConsDecls cs = vsep $
  zipWith (<+>) (equals : repeat (char '|')) (map ppConsDecl cs)

--- pretty print a single constructor
ppConsDecl :: ConsDecl -> Doc
ppConsDecl (Cons qn _ _ tys) = hsep $ ppPrefixOp qn : map (ppTypeExpr 2) tys

--- pretty-print a type expression
ppTypeExpr :: Int -> TypeExpr -> Doc
ppTypeExpr _ (TVar           v) = ppTVarIndex v
ppTypeExpr p (FuncType ty1 ty2) = parenIf (p > 0) $
  ppTypeExpr 1 ty1 </> text "->" <+> ppTypeExpr 0 ty2
ppTypeExpr p (TCons     qn tys)
  | isListId qn && length tys == 1 = brackets (ppTypeExpr 0 (head tys))
  | isTupleId qn                   = tupled   (map (ppTypeExpr 0) tys)
  | otherwise                      = parenIf (p > 1 && not (null tys)) $ sep
                                     (ppPrefixOp qn : map (ppTypeExpr 2) tys)

--- pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ chr c : if n == 0 then [] else show n
               | n <- [0 ..], c <- [ord 'a' .. ord 'z']
               ]

--- pretty-print a list of function declarations
ppFuncDecls :: [FuncDecl] -> Doc
ppFuncDecls []       = empty
ppFuncDecls fs@(_:_) = vsep (map ((<> line) . ppFuncDecl) fs)

--- pretty-print a function declaration
ppFuncDecl :: FuncDecl -> Doc
ppFuncDecl (Func qn _ _ ty r)
  =   indent (sep [ppPrefixOp qn, text "::", ppTypeExpr 0 ty])
  <$> indent (ppPrefixOp qn <+> ppRule r)

--- pretty-print a function rule
ppRule :: Rule -> Doc
ppRule (Rule  vs e) = hsep (map ppVarIndex vs) </> equals <+> ppExpr 0 e
ppRule (External e) = text "external" <+> dquotes (text e)

--- pretty-print an expression
ppExpr :: Int -> Expr -> Doc
ppExpr _ (Var        v) = ppVarIndex v
ppExpr _ (Lit        l) = ppLiteral l
ppExpr p (Comb _ qn es) = ppComb p qn es
ppExpr p (Free    vs e)
  | null vs             = ppExpr p e
  | otherwise           = parenIf (p > 0) $ sep
                          [ text "let"
                            <+> encloseSep empty empty comma (map ppVarIndex vs)
                            <+> text "free"
                          , text "in" </> ppExpr 0 e
                          ]
ppExpr p (Let     ds e) = parenIf (p > 0) $ sep
                          [text "let" <+> ppDecls ds, text "in"  <+> ppExpr 0 e]
ppExpr p (Or     e1 e2) = parenIf (p > 0)
                        $ ppExpr 1 e1 <+> text "?" <+> ppExpr 1 e2
ppExpr p (Case ct e bs) = parenIf (p > 0) $ indent
                        $ ppCaseType ct <+> ppExpr 0 e <+> text "of"
                          <$> vsep (map ppBranch bs)
ppExpr p (Typed   e ty) = parenIf (p > 0)
                        $ ppExpr 0 e <+> text "::" <+> ppTypeExpr 0 ty

--- pretty-print a variable
ppVarIndex :: VarIndex -> Doc
ppVarIndex i = text $ if i < 0 then "_" else 'v' : show i

--- pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = int i
ppLiteral (Floatc f) = float f
ppLiteral (Charc  c) = text (showEscape c)

--- Escape character literal
showEscape :: Char -> String
showEscape c
  | o <   10  = "'\\00" ++ show o ++ "'"
  | o <   32  = "'\\0"  ++ show o ++ "'"
  | o == 127  = "'\\127'"
  | otherwise = show c
  where o = ord c

--- Pretty print a constructor or function call
ppComb :: Int -> QName -> [Expr] -> Doc
ppComb p qn es | isListId  qn && null es = text "[]"
               | isTupleId qn            = tupled (map (ppExpr 0) es)
               | otherwise               = case es of
  []               -> ppPrefixOp qn
  [e1,e2]
    | isConsId  qn -> parenIf (p > 0)
                    $ sep [ppExpr 1 e1, text ":", ppExpr 1 e2]
    | isInfixOp qn -> parenIf (p > 0)
                    $ sep [ppExpr 1 e1, ppInfixOp qn, ppExpr 1 e2]
  _                -> parenIf (p > 0)
                    $ sep (ppPrefixOp qn : map (ppExpr 1) es)

--- pretty-print a list of declarations
ppDecls :: [(VarIndex, Expr)] -> Doc
ppDecls = vsep . map ppDecl

--- pretty-print a single declaration
ppDecl :: (VarIndex, Expr) -> Doc
ppDecl (v, e) = ppVarIndex v <+> equals <+> ppExpr 0 e

--- Pretty print the type of a case expression
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

--- Pretty print a case branch
ppBranch :: BranchExpr -> Doc
ppBranch (Branch p e) = ppPattern p <+> text "->" <+> indent (ppExpr 0 e)

--- Pretty print a pattern
ppPattern :: Pattern -> Doc
ppPattern (Pattern c vs)
  | isListId c && null vs = text "[]"
  | isTupleId c           = tupled (map ppVarIndex vs)
  | otherwise             = case vs of
  [v1,v2] | isConsId  c -> ppVarIndex v1 <+> text ":"    <+> ppVarIndex v2
          | isInfixOp c -> ppVarIndex v1 <+> ppInfixOp c <+> ppVarIndex v2
  _                     -> hsep (ppPrefixOp c : map ppVarIndex vs)
ppPattern (LPattern   l) = ppLiteral l

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

--- pretty-print a prefix operator
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parenIf (isInfixOp qn) (ppQName qn)

--- pretty-print an infix operator
ppInfixOp :: QName -> Doc
ppInfixOp qn = if isInfixOp qn then ppQName qn else bquotes (ppQName qn)

--- Pretty-print a qualified name
ppQName :: QName -> Doc
ppQName (m, i) = text $ m ++ '.' : i

--- Check whether an operator is an infix operator
isInfixOp :: QName -> Bool
isInfixOp = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents a list
isListId :: QName -> Bool
isListId (m, i) = m `elem` ["Prelude", ""] && i == "[]"

--- Check whether an identifier represents the list constructor `:`
isConsId :: QName -> Bool
isConsId (m, i) = m `elem` ["Prelude", ""] && i == ":"

--- Check whether an identifier represents a tuple
isTupleId :: QName -> Bool
isTupleId (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

--- Surround with parentheses depending on the flag
parenIf :: Bool -> Doc -> Doc
parenIf b s = if b then parens s else s

--- Indentation
indent :: Doc -> Doc
indent = nest 2
