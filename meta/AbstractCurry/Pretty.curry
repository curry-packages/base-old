--- --------------------------------------------------------------------------
--- Pretty printing of AbstractCurry.
---
--- This library provides a pretty-printer for AbstractCurry modules.
---
--- @author  Yannik Potdevin
--- @version June 2015
--- --------------------------------------------------------------------------
module AbstractCurry.Pretty (Options, Qualification(..), defaultOptions, printCurryProg) where

import Pretty
import AbstractCurry

data Options = Options { pageWidth        :: Int
                       , indentationWidth :: Int
                       , qualification    :: Qualification
                       , moduleName       :: String
                       }

data Qualification
    = Full      -- ^ Fully qualify every function, including those of the
                --   processed module and Prelude
    | Imports   -- ^ Fully qualify external functions, do not qualify local
                --   functions and those of Prelude
    | None      -- ^ Do not qualify any function

options :: Int              -- ^ page width
        -> Int              -- ^ indentation width
        -> Qualification    -- ^ what names to qualify
        -> MName            -- ^ the name of current module
        -> Options
options pw iw q m = Options { pageWidth        = pw
                            , indentationWidth = iw
                            , qualification    = q
                            , moduleName       = m }

defaultOptions :: Options
defaultOptions = options 80 4 Imports ""

printCurryProg :: Options -> CurryProg -> String
printCurryProg opts cprog = pretty (pageWidth opts) $ ppCurryProg opts cprog

--- pretty-print a CurryProg (the representation of a program, written in curry,
--- using AbstractCurry) according to given options. This function will overwrite
--- the module name given by options with the name encapsulated in CurryProg.
ppCurryProg :: Options -> CurryProg -> Doc
ppCurryProg opts (CurryProg m ms ts fs os)
    = text "module" <+> ppMName opts' m
  <$> indent' opts' (ppExports opts' exports <+> where_)
 <$+> ppImports opts' ms
 <$+> vcatMap (ppCOpDecl opts') os
 <$+> vcatMap (ppCTypeDecl opts') ts
 <$+> vsepBlankMap (ppCFuncDecl opts') fs
    where exports = getExports opts' ts fs
          opts'   = opts { moduleName = m }

--- pretty-print a module name (just a string) according to given options.
ppMName :: Options -> MName -> Doc
ppMName _ = text

--- pretty-print exports, i.e. all type  and function declarations which are
--- public.
ppExports :: Options -> [Doc] -> Doc
ppExports _ []       = empty
ppExports _ ds@(_:_) = tupledSpaced ds

--- pretty-print imports (list of module names) by prepending the word "import"
--- to the module name.
ppImports :: Options -> [MName] -> Doc
ppImports opts = vcatMap (\m -> text "import" <+> ppMName opts m)

--- pretty-print operator precedence declarations.
ppCOpDecl :: Options -> COpDecl -> Doc
ppCOpDecl opts (COp qn fix p) = ppCFixity fix
                             <+> int p
                             <+> backticksIf (not $ isInfixOp qn) (ppQName opts qn)

--- pretty-print the fixity of a function.
ppCFixity :: CFixity -> Doc
ppCFixity CInfixOp  = text "infix"
ppCFixity CInfixlOp = text "infixl"
ppCFixity CInfixrOp = text "infixr"

--- pretty-print type declarations, like `data ... = ...`, `type ... = ...` or
--- `newtype ... = ...`.
ppCTypeDecl :: Options -> CTypeDecl -> Doc
ppCTypeDecl opts (CType qn _ tVars cDecls)
    | null cDecls = prefix
    | null tVars  = prefix <+> suffix
    | otherwise   = prefix <+> ppCTVarINames opts tVars
                           <+> suffix
    where prefix = text "data" <+> ppQName opts qn
          suffix = ppCConsDecls opts cDecls
ppCTypeDecl opts (CTypeSyn qn _ tVars tExp)
    | null tVars = prefix <+> suffix
    | otherwise  = prefix <+> ppCTVarINames opts tVars <+> suffix
    where prefix = text "type" <+> ppQName opts qn
          suffix = equals <+> ppCTypeExpr opts tExp
ppCTypeDecl opts (CNewType qn _ tVars cDecl)
    = text "newtype" <+> ppQName opts qn <+> ppCTVarINames opts tVars
                     <+> equals <+> ppCConsDecl opts cDecl

--- pretty-print a list of constructor declarations, including the `=` sign.
ppCConsDecls :: Options -> [CConsDecl] -> Doc
ppCConsDecls opts cs
    = equals <+> fillEncloseSepSpaced empty empty bar (map (ppCConsDecl opts) cs)

--- pretty-print a constructor declaration.
ppCConsDecl :: Options -> CConsDecl -> Doc
ppCConsDecl opts (CCons   qn _ tExps)  =  ppQName opts qn
                                      <+> hsepMap (ppCTypeExpr opts) tExps
ppCConsDecl opts (CRecord qn _ fDecls)
    = ppQName opts qn <+> setSpaced (map (ppCFieldDecl opts) fDecls)

--- pretty-print a record field declaration (`field :: type`).
ppCFieldDecl :: Options -> CFieldDecl -> Doc
ppCFieldDecl opts (CField qn _ tExp) = ppQName opts qn <+> doubleColon
                                                       <+> ppCTypeExpr opts tExp

--- pretty-print a function declaration.
ppCFuncDecl :: Options -> CFuncDecl -> Doc
ppCFuncDecl opts (CFunc qn _ _ tExp rs)
    = ppCFuncSignature opts qn tExp <$$> ppCRules opts qn rs
ppCFuncDecl opts (CmtFunc cmt qn a v tExp rs)
    = string cmt <$$> ppCFuncDecl opts (CFunc qn a v tExp rs)

--- pretty-print a function signature according to given options.
ppCFuncSignature :: Options -> QName -> CTypeExpr -> Doc
ppCFuncSignature opts qn tExp
    | isInfixOp qn = parens prefix <+> suffix
    | otherwise    = prefix <+> suffix
    where prefix = ppQName opts qn
          suffix = doubleColon <+> ppCTypeExpr opts tExp

--- pretty-print a type expression.
ppCTypeExpr :: Options -> CTypeExpr -> Doc
ppCTypeExpr opts (CTVar     tvar)        =  ppCTVarIName opts tvar
ppCTypeExpr opts (CFuncType tExp1 tExp2) =
    case tExp1 of (CFuncType _ _) -> parens prefix <+> suffix -- `->` is right-associative
                  _               -> prefix <+> suffix
    where prefix = ppCTypeExpr opts tExp1
          suffix = arrow <+> ppCTypeExpr opts tExp2
ppCTypeExpr opts (CTCons qn tExps)
    | null tExps     = pqn
    | isListCons qn  = brackets $ head expDocs -- this relies on expDocs being a singleton
    | isTupleCons qn = tupledSpaced expDocs
    | otherwise      = pqn <+> hsep expDocs
    where pqn     = ppQName opts qn
          expDocs = map (ppCTypeExpr opts) tExps

--- pretty-print a list of type variables horizontally separating them by `space`.
ppCTVarINames :: Options -> [CTVarIName] -> Doc
ppCTVarINames opts = hsepMap (ppCTVarIName opts)

--- pretty-print a type variable (currently the Int is ignored).
ppCTVarIName :: Options -> CTVarIName -> Doc
ppCTVarIName _ (_, tvar) = text tvar

--- pretty-print a list of function rules, concatenated vertically, prepending
--- the name of the function (second argument) in each rule.
ppCRules :: Options -> QName -> [CRule] -> Doc
ppCRules opts qn = vcatMap ((ppQName opts qn <+>) . ppCRule opts)

--- pretty-print a rule of a function. Given a function
--- `f x y = x * y`, then `x y = x * y` is a rule consisting of `x y` as list of
--- patterns and `x * y` as right hand side.
ppCRule :: Options -> CRule -> Doc
ppCRule opts (CRule ps rhs) =  hsepMap (ppCPattern opts) ps
                           <+> ppCRhs opts equals rhs

-- TODO: Handling of any non prefix constructor pattern, nesting
ppCPattern :: Options -> CPattern -> Doc
ppCPattern opts (CPVar      pvar)   =  ppCVarIName opts pvar
ppCPattern opts (CPLit      lit)    =  ppCLiteral opts lit
ppCPattern opts (CPComb     qn ps)  =  parens $ ppQName opts qn <+> hsepMap (ppCPattern opts) ps
ppCPattern opts (CPAs       pvar p) =  ppCVarIName opts pvar <> at <> parens (ppCPattern opts p)
ppCPattern opts (CPFuncComb qn ps)  =  parens $ ppQName opts qn <+> hsepMap (ppCPattern opts) ps -- TODO
ppCPattern opts (CPLazy     p)      =  tilde <> parens (ppCPattern opts p)
ppCPattern opts (CPRecord   qn rps) =  ppQName opts qn
                                   <+> setSpaced (map (ppCFieldPattern opts) rps)

--- pretty-print a pattern variable (currently the Int is ignored).
ppCVarIName :: Options -> CVarIName -> Doc
ppCVarIName _ (_, pvar) = text pvar

--- pretty-print given literal (Int, Float, ...).
ppCLiteral :: Options -> CLiteral -> Doc
ppCLiteral _ (CIntc i)    = int i
ppCLiteral _ (CFloatc f)  = float f
ppCLiteral _ (CCharc c)   = text $ show c
ppCLiteral _ (CStringc s) = text $ show s -- TODO: Escape sequences

ppCFieldPattern :: Options -> CField CPattern -> Doc
ppCFieldPattern opts (qn, p) = ppQName opts qn <+> equals <+> ppCPattern opts p

--- pretty-print the right hand side of a rule (or case expression), including
--- the d sign, where `d` is the relation (as doc) between the left hand side
--- and the right hand side -- usually this is one of `=`, `->`.
ppCRhs :: Options -> Doc -> CRhs -> Doc
ppCRhs opts d (CSimpleRhs  exp lDecls)
    | null lDecls = sharedPrefix
    | otherwise   = sharedPrefix <$$> indent' opts
                                              (ppCLocalDecls opts where_ lDecls)
    where sharedPrefix = d <+> ppCExpr opts exp
ppCRhs opts d (CGuardedRhs conds lDecls)
    | null lDecls = sharedPrefix
    | otherwise   = sharedPrefix <$$> indent' opts
                                              (ppCLocalDecls opts where_ lDecls)
    where sharedPrefix = ppCGuardedRhs opts d conds

--- pretty-print guard, i.e. the `| cond d exp` part of a right hand side, where
--- `d` is the relation (as doc) between `cond` and `exp` -- usually this is
--- one of `=`, `->`.
ppCGuardedRhs :: Options -> Doc -> [(CExpr, CExpr)] -> Doc
ppCGuardedRhs opts d = align . vcatMap (ppCGuard opts)
    where ppCGuard opts (e1, e2) = bar <+> ppCExpr opts e1
                                       <+> d
                                       <+> ppCExpr opts e2

--- pretty-print local declarations . If the second argument is `text "where"`,
--- pretty-print a `where` block. If the second argument is `text "let"`,
--- pretty-print a `let` block without `in`.
ppCLocalDecls :: Options -> Doc -> [CLocalDecl] -> Doc
-- ppCLocalDecls opts d lDecls = d <+> nest 0 (align (vcatMap (ppCLocalDecl opts) lDecls))
ppCLocalDecls opts d lDecls = d <+> align (vcatMap (ppCLocalDecl opts) lDecls)

--- pretty-print local declarations (the part that follows the `where` keyword).
ppCLocalDecl :: Options -> CLocalDecl -> Doc
ppCLocalDecl opts (CLocalFunc fDecl) = ppCFuncDecl opts fDecl
ppCLocalDecl opts (CLocalPat  p rhs) = ppCPattern opts p <+> ppCRhs opts equals rhs
ppCLocalDecl opts (CLocalVars pvars)
    = hsep $ punctuate comma $ map (ppCVarIName opts) pvars

--- pretty-print an expression.
ppCExpr :: Options -> CExpr -> Doc
ppCExpr opts (CVar     pvar)   = ppCVarIName opts pvar
ppCExpr opts (CLit     lit)    = ppCLiteral opts lit
ppCExpr opts (CSymbol  qn)     = ppQName opts qn
ppCExpr opts (CApply   e1 e2)  = ppCExpr opts e1 <+> ppCExpr opts e2
ppCExpr opts (CLambda  ps exp) =  backslash <> hsepMap (ppCPattern opts) ps
                              <+> arrow <+> ppCExpr opts exp
ppCExpr opts (CLetDecl lDecls exp) =  align $ ppCLocalDecls opts let_ lDecls
                                 <$$> text "in" <+> ppCExpr opts exp
ppCExpr opts (CDoExpr stms) =  text "do"
                           <+> align (vcatMap (ppCStatement opts) stms)
ppCExpr opts (CListComp exp stms) =  brackets $ ppCExpr opts exp
                                 <+> bar
                                 <+> fillSep
                                     (punctuate (comma <> space)
                                                (map (ppCStatement opts) stms))
ppCExpr opts (CCase cType exp cases) =  ppCCaseType cType
                                    <+> ppCExpr opts exp
                                    <+> text "of"
                                    <+> ppCases opts cases
ppCExpr opts (CTyped exp tExp) =  ppCExpr opts exp
                              <+> doubleColon
                              <+> ppCTypeExpr opts tExp
ppCExpr opts (CRecConstr qn rFields) =  ppQName opts qn
                                    <+> ppRecordFields opts rFields
ppCExpr opts (CRecUpdate exp rFields) =  ppCExpr opts exp
                                     <+> ppRecordFields opts rFields

ppCStatement :: Options -> CStatement -> Doc
ppCStatement opts (CSExpr exp)     =  ppCExpr opts exp
ppCStatement opts (CSPat  pat exp) =  ppCPattern opts pat
                                  <+> larrow <+> ppCExpr opts exp
ppCStatement opts (CSLet  lDecls)  = ppCLocalDecls opts let_ lDecls

--- pretty-print `case`, `fcase` keywords.
ppCCaseType :: CCaseType -> Doc
ppCCaseType CRigid = text "case"
ppCCaseType CFlex  = text "fcase"

--- pretty-print a list of case expressions, i.e. the `p1 -> e1`, …, `pn -> en`,
--- transitions, vertically aligned.
ppCases :: Options -> [(CPattern, CRhs)] -> Doc
ppCases opts = align . vcatMap (ppCase opts)

--- pretty-print a case expression.
ppCase :: Options -> (CPattern, CRhs) -> Doc
ppCase opts (p, rhs) = ppCPattern opts p <+> ppCRhs opts arrow rhs

--- pretty-print record field assignments like this:
---     { lab1 = exp1, ..., labn expn }
--- if it fits the page, or
---     { lab1 = exp1
---     , …
---     , labn = expn }
--- otherwise.
ppRecordFields :: Options -> [CField CExpr] -> Doc
ppRecordFields opts = setSpaced . map (ppRecordField opts)

--- pretty-print a record field assignment (`fieldLabel = exp`).
ppRecordField :: Options -> CField CExpr -> Doc
ppRecordField opts (qn, exp) = ppQName opts qn <+> equals <+> ppCExpr opts exp

--- extract the type and function declarations which are public and gather their
--- qualified names in a list.
getExports :: Options -> [CTypeDecl] -> [CFuncDecl] -> [Doc]
getExports opts ts fs = map tDeclToDoc filteredTs ++ map fDeclToDoc filteredFs
    where tDeclToDoc (CType    qn _ _ _) = ppQName' qn
          tDeclToDoc (CTypeSyn qn _ _ _) = ppQName' qn
          tDeclToDoc (CNewType qn _ _ _) = ppQName' qn

          fDeclToDoc (CFunc     qn _ _ _ _) = ppQName' qn
          fDeclToDoc (CmtFunc _ qn _ _ _ _) = ppQName' qn

          filteredTs = filter (\t -> case t of (CType    _ Public _ _) -> True
                                               (CTypeSyn _ Public _ _) -> True
                                               (CNewType _ Public _ _) -> True
                                               _                       -> False) ts
          filteredFs = filter (\f -> case f of (CFunc     _ _ Public _ _) -> True
                                               (CmtFunc _ _ _ Public _ _) -> True
                                               _                          -> False) fs
          ppQName' qn = parensIf (isInfixOp qn) $ ppQName opts qn

--- pretty-print a QName according to given options (how to qualify).
ppQName :: Options -> QName -> Doc
ppQName opts (m, f)
    = case qualification opts
           of Full    -> preparedFQName
              Imports -> if moduleName opts == m || "Prelude" == m
                            then preparedName
                            else preparedFQName
              None    -> preparedName
    where preparedFQName = ppMName opts m <> dot <> preparedName
          preparedName   = text f

-- Helping function (diagnosis)
--- Check whether an operator is an infix operator
isInfixOp :: QName -> Bool
isInfixOp = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents a list
isListCons :: QName -> Bool
isListCons (m, i) = m `elem` ["Prelude", ""] && i == "[]"

--- Check whether an identifier represents the list constructor `:`
isConsCons :: QName -> Bool
isConsCons (m, i) = m `elem` ["Prelude", ""] && i == ":"

--- Check whether an identifier represents a tuple
isTupleCons :: QName -> Bool
isTupleCons (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

-- Helping functions (pretty printing)
vsepBlankMap :: (a -> Doc) -> [a] -> Doc
vsepBlankMap f = vsepBlank . map f

indent' :: Options -> Doc -> Doc
indent' opts = indent (indentationWidth opts)

backtick :: Doc
backtick = char '`'

backticks :: Doc -> Doc
backticks = enclose backtick backtick

backticksIf :: Bool -> Doc -> Doc
backticksIf b d = if b then backticks d else d

larrow :: Doc
larrow = text "<-"

where_ :: Doc
where_ = text "where"

let_ :: Doc
let_ = text "let"