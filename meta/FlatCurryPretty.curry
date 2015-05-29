--- --------------------------------------------------------------------------
--- Pretty printing of FlatCurry.
---
--- This library provides pretty-printers for FlatCurry modules and all
--- substructures.
---
--- @author  Björn Peemöller
--- @version May 2015
--- --------------------------------------------------------------------------
module FlatCurryPretty where

import Pretty

import FlatCurry

--- pretty-print a FlatCurry module
ppProg :: Prog -> Doc
ppProg (Prog m is ts fs os) = compose ($+$) -- adds blank line in between
  [ ppHeader    m ts fs
  , ppImports   is
  , ppOpDecls   os
  , ppTypeDecls ts
  , ppFuncDecls fs
  ]

--- pretty-print the module header
ppHeader :: String -> [TypeDecl] -> [FuncDecl] -> Doc
ppHeader m ts fs
  = indent (sep [text "module" <+> text m, ppExports ts fs, text "where"])

--- pretty-print the export list
ppExports :: [TypeDecl] -> [FuncDecl] -> Doc
ppExports ts fs = tupledSpaced (map ppTypeExport ts ++ ppFuncExports fs)

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
ppImports is = vsep (map (indent . ppImport) is)
  where ppImport m = text "import" <+> text m

--- pretty-print a list of operator fixity declarations
ppOpDecls :: [OpDecl] -> Doc
ppOpDecls os = vsep (map (indent . ppOpDecl) os)
  where ppOpDecl (Op qn fix n) = ppFixity fix <+> int n <+> ppInfixOp qn

--- pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

--- pretty-print a list of type declarations
ppTypeDecls :: [TypeDecl] -> Doc
ppTypeDecls ts = vsep (map (indent . ppTypeDecl) ts)

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

--- pretty a top-level type expression
ppTypeExp :: TypeExpr -> Doc
ppTypeExp = ppTypeExpr 0

--- pretty-print a type expression
ppTypeExpr :: Int -> TypeExpr -> Doc
ppTypeExpr _ (TVar           v) = ppTVarIndex v
ppTypeExpr p (FuncType ty1 ty2) = parensIf (p > 0) $
  ppTypeExpr 1 ty1 </> rarrow <+> ppTypeExp ty2
ppTypeExpr p (TCons     qn tys)
  | isListId qn && length tys == 1 = brackets (ppTypeExp (head tys))
  | isTupleId qn                   = tupled   (map ppTypeExp tys)
  | otherwise                      = parensIf (p > 1 && not (null tys)) $ sep
                                     (ppPrefixOp qn : map (ppTypeExpr 2) tys)

--- pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ chr c : if n == 0 then [] else show n
               | n <- [0 ..], c <- [ord 'a' .. ord 'z']
               ]

--- pretty-print a list of function declarations
ppFuncDecls :: [FuncDecl] -> Doc
ppFuncDecls fs = compose ($+$) (map ppFuncDecl fs)

--- pretty-print a function declaration
ppFuncDecl :: FuncDecl -> Doc
ppFuncDecl (Func qn _ _ ty r)
  =  indent (sep [ppPrefixOp qn, text "::", ppTypeExp ty])
  $$ indent (ppPrefixOp qn <+> ppRule r)

--- pretty-print a function rule
ppRule :: Rule -> Doc
ppRule (Rule  vs e)
  | null vs   = equals <+> ppExp e
  | otherwise = hsep (map ppVarIndex vs) </> equals <+> ppExp e
ppRule (External e) = text "external" <+> dquotes (text e)

--- Pretty-print a top-level expression.
ppExp :: Expr -> Doc
ppExp = ppExpr 0

--- pretty-print an expression
ppExpr :: Int -> Expr -> Doc
ppExpr _ (Var        v) = ppVarIndex v
ppExpr _ (Lit        l) = ppLiteral l
ppExpr p (Comb _ qn es) = ppComb p qn es
ppExpr p (Free    vs e)
  | null vs             = ppExpr p e
  | otherwise           = parensIf (p > 0) $ sep
                          [ text "let"
                            <+> encloseSep empty empty comma (map ppVarIndex vs)
                            <+> text "free"
                          , text "in" </> ppExp e
                          ]
ppExpr p (Let     ds e) = parensIf (p > 0) $ sep
                          [text "let" <+> ppDecls ds, text "in" <+> ppExp e]
ppExpr p (Or     e1 e2) = parensIf (p > 0)
                        $ ppExpr 1 e1 <+> text "?" <+> ppExpr 1 e2
ppExpr p (Case ct e bs) = parensIf (p > 0) $ indent
                        $ ppCaseType ct <+> ppExpr 1 e <+> text "of"
                          $$ vsep (map ppBranch bs)
ppExpr p (Typed   e ty) = parensIf (p > 0)
                        $ ppExp e <+> text "::" <+> ppTypeExp ty

--- pretty-print a variable
ppVarIndex :: VarIndex -> Doc
ppVarIndex i = text $ if i < 0 then "_" else 'v' : show i

--- pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = int i
ppLiteral (Floatc f) = float f
ppLiteral (Charc  c) = text (show c)

--- Pretty print a constructor or function call
ppComb :: Int -> QName -> [Expr] -> Doc
ppComb p qn es | isListId  qn && null es = text "[]"
               | isTupleId qn            = tupled (map ppExp es)
               | otherwise               = case es of
  []               -> ppPrefixOp qn
  [e1,e2]
    | isInfixOp qn -> parensIf (p > 0)
                    $ sep [ppExpr 1 e1, ppInfixOp qn, ppExpr 1 e2]
  _                -> parensIf (p > 0)
                    $ sep (ppPrefixOp qn : map (ppExpr 1) es)

--- pretty-print a list of declarations
ppDecls :: [(VarIndex, Expr)] -> Doc
ppDecls = vsep . map ppDecl

--- pretty-print a single declaration
ppDecl :: (VarIndex, Expr) -> Doc
ppDecl (v, e) = ppVarIndex v <+> equals <+> ppExp e

--- Pretty print the type of a case expression
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

--- Pretty print a case branch
ppBranch :: BranchExpr -> Doc
ppBranch (Branch p e) = ppPattern p <+> rarrow <+> indent (ppExp e)

--- Pretty print a pattern
ppPattern :: Pattern -> Doc
ppPattern (Pattern c vs)
  | isListId c && null vs = text "[]"
  | isTupleId c           = tupled (map ppVarIndex vs)
  | otherwise             = case vs of
  [v1,v2] | isInfixOp c -> ppVarIndex v1 <+> ppInfixOp c <+> ppVarIndex v2
  _                     -> hsep (ppPrefixOp c : map ppVarIndex vs)
ppPattern (LPattern   l) = ppLiteral l

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

--- pretty-print a prefix operator
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parensIf (isInfixOp qn) (ppQName qn)

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

--- Check whether an identifier represents a tuple
isTupleId :: QName -> Bool
isTupleId (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

--- Indentation
indent :: Doc -> Doc
indent = nest 2
