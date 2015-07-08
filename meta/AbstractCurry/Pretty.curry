--- --------------------------------------------------------------------------
--- Pretty printing of AbstractCurry.
---
--- This library provides a pretty-printer for AbstractCurry modules.
---
--- @author  Yannik Potdevin
--- @version July 2015
--- --------------------------------------------------------------------------
module AbstractCurry.Pretty where

import Pretty
import AbstractCurry
import Maybe (isJust, fromJust)

data Qualification
    = Full      -- ^ Fully qualify every function, including those of the
                --   processed module and Prelude
    | Imports   -- ^ Fully qualify external functions, do not qualify local
                --   functions and those of Prelude
    | None      -- ^ Do not qualify any function

data Options = Options { pageWidth        :: Int
                       , indentationWidth :: Int
                       , qualification    :: Qualification
                       , moduleName       :: String }

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
defaultOptions = options 80 4 Full ""

--- precedence of top level (pattern or application) context -- lowest
tlPrec      :: Int
tlPrec      = 0
--- precedence of infix (pattern or application) context
infAppPrec  :: Int
infAppPrec  = 1
--- precedence of standard prefix (pattern or application) context
prefAppPrec :: Int
prefAppPrec = 2
--- precedence of atoms (variables, literals, tuples, lists ...)
highestPrec :: Int
highestPrec = 3

printCurryProg :: Options -> CurryProg -> String
printCurryProg opts cprog = pretty (pageWidth opts) $ ppCurryProg opts cprog

--- pretty-print a CurryProg (the representation of a program, written in curry,
--- using AbstractCurry) according to given options. This function will overwrite
--- the module name given by options with the name encapsulated in CurryProg.
ppCurryProg :: Options -> CurryProg -> Doc
ppCurryProg opts (CurryProg m ms ts fs os)
    = text "module" <+> ppMName opts' m
   $$ indent' opts' (ppExports opts' exports </> where_)
  $+$ ppImports opts' ms
  $+$ vcatMap (ppCOpDecl opts') os
  $+$ vcatMap (ppCTypeDecl opts') ts
  $+$ vsepBlankMap (ppCFuncDecl opts') fs
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
ppCOpDecl _ (COp qn fix p) = ppCFixity fix
                         <+> int p
                         <+> bquotesIf (not $ isInfixId qn) (ppName qn)

--- pretty-print the fixity of a function.
ppCFixity :: CFixity -> Doc
ppCFixity CInfixOp  = text "infix"
ppCFixity CInfixlOp = text "infixl"
ppCFixity CInfixrOp = text "infixr"

--- pretty-print type declarations, like `data ... = ...`, `type ... = ...` or
--- `newtype ... = ...`.
ppCTypeDecl :: Options -> CTypeDecl -> Doc
ppCTypeDecl opts (CType qn _ tVars cDecls)
    = text "data" <+> ppName qn
 <++> if null cDecls
         then empty
         else ppCTVarINames opts tVars <++> ppCConsDecls opts cDecls
ppCTypeDecl opts (CTypeSyn qn _ tVars tExp)
    = text "type" <+> ppName qn
 <++> ppCTVarINames opts tVars
 <++> equals <+> ppCTypeExpr opts tExp
ppCTypeDecl opts (CNewType qn _ tVars cDecl)
    = text "newtype" <+> ppName qn
 <++> ppCTVarINames opts tVars
 <++> equals <+> ppCConsDecl opts cDecl

--- pretty-print a list of constructor declarations, including the `=` sign.
ppCConsDecls :: Options -> [CConsDecl] -> Doc
ppCConsDecls opts cs
    = fillEncloseSepSpaced equals empty (space <> bar) (map (ppCConsDecl opts) cs)

--- pretty-print a constructor declaration.
ppCConsDecl :: Options -> CConsDecl -> Doc
ppCConsDecl opts (CCons   qn _ tExps ) = ppName qn
                                    <++> hsepMap (ppCTypeExpr opts) tExps
ppCConsDecl opts (CRecord qn _ fDecls) = ppName qn
                                     <+> setSpaced (map (ppCFieldDecl opts)
                                                        fDecls)

--- pretty-print a record field declaration (`field :: type`).
ppCFieldDecl :: Options -> CFieldDecl -> Doc
ppCFieldDecl opts (CField qn _ tExp) = ppName qn
                                   <+> doubleColon
                                   <+> ppCTypeExpr opts tExp

--- pretty-print a function declaration.
ppCFuncDecl :: Options -> CFuncDecl -> Doc
ppCFuncDecl opts (CFunc qn _ _ tExp rs)
    = ppCFuncSignature opts qn tExp <$$> ppCRules opts qn rs
ppCFuncDecl opts (CmtFunc cmt qn a v tExp rs)
    = string cmt $$ ppCFuncDecl opts (CFunc qn a v tExp rs)

--- pretty-print a function signature according to given options.
ppCFuncSignature :: Options -> QName -> CTypeExpr -> Doc
ppCFuncSignature opts qn tExp = parensIf (isInfixId qn) (ppName qn)
                            <+> doubleColon
                            <+> ppCTypeExpr opts tExp

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

--- pretty-print a list of function rules, concatenated vertically.
ppCRules :: Options -> QName -> [CRule] -> Doc
ppCRules opts qn rs = case rs of
                           [] -> parensIf (isInfixId qn) (ppName qn)
                             <+> text "external"
                           _  -> vcatMap (ppCRule opts qn) rs

--- pretty-print a rule of a function. Given a function
--- `f x y = x * y`, then `x y = x * y` is a rule consisting of `x y` as list of
--- patterns and `x * y` as right hand side.
ppCRule :: Options -> QName -> CRule -> Doc
ppCRule opts qn (CRule ps rhs) = hsep (positionIdent ppName qn pDocs)
                             <+> ppCRhs opts equals rhs
    where pDocs = map (ppCPattern' prefAppPrec opts) ps

--- pretty-print a pattern expression.
ppCPattern :: Options -> CPattern -> Doc
ppCPattern = ppCPattern' tlPrec

-- Internal use only: Pretty-print a pattern expression and make use of supplied
-- precedence context. The supplied number represents the precedence of the
-- enclosing pattern. Higher values mean more precedence, so if the nested
-- pattern has lower precedence than the enclosing pattern, the nested one has
-- to be enclosed in parentheses.
ppCPattern' :: Int -> Options -> CPattern -> Doc
ppCPattern' _ opts (CPVar  pvar   ) = ppCVarIName opts pvar
ppCPattern' _ opts (CPLit  lit    ) = ppCLiteral opts lit
ppCPattern' p opts (CPComb qn   ps) = ppCPComb p opts qn ps
ppCPattern' _ opts (CPAs   pvar p ) = ppCVarIName opts pvar
                                   <> at
                                   <> ppCPattern' highestPrec opts p
ppCPattern' p opts (CPFuncComb qn ps ) =
    case ps of
         [p1, p2] | isInfixId qn -> parensIf (p >= infAppPrec)
                                  $ ppCPattern' infAppPrec opts p1
                                <+> ppQName opts qn
                                <+> ppCPattern' infAppPrec opts p2
         _                       -> parensIf (p >= prefAppPrec)
                                  $ ppQName opts qn
                                <+> hsepMap (ppCPattern' prefAppPrec opts) ps
ppCPattern' _ opts (CPLazy     p     ) = tilde <> ppCPattern' highestPrec opts p
ppCPattern' _ opts (CPRecord   qn rps) = ppQName opts qn
                                     <+> setSpaced (map (ppCFieldPattern opts)
                                                        rps)

-- pretty print the application of an n-ary constructor.
ppCPComb :: Int -> Options -> QName -> [CPattern] -> Doc
ppCPComb p opts qn ps =
    case pDocs of
         [cons]                    -> cons
         [l, m, r] |    m == colon
                     && r == nil   -> brackets l
                   | isInfixId qn  -> parensIf (p >= infAppPrec) $ l <+> m <+> r
                                      {- assume tupled pattern and therefore
                                         avoid additional parenthesis. -}
         (x:_) | x == lparen       -> hsep pDocs
         _                         -> parensIf (p >= prefAppPrec) $ hsep pDocs
    where pDocs = positionIdent (ppQName opts) qn
                $ map (ppCPattern' p' opts) ps
          p'    = if isInfixId qn
                     then infAppPrec
                     else prefAppPrec {- this assumes the existence of infix
                                         constructors and all of them having a
                                         lower precedence than any prefix
                                         constructors. -}

--- pretty-print a pattern variable (currently the Int is ignored).
ppCVarIName :: Options -> CVarIName -> Doc
ppCVarIName _ (_, pvar) = text pvar

--- pretty-print given literal (Int, Float, ...).
ppCLiteral :: Options -> CLiteral -> Doc
ppCLiteral _ (CIntc i)    = int i
ppCLiteral _ (CFloatc f)  = float f
ppCLiteral _ (CCharc c)   = text $ show c
ppCLiteral _ (CStringc s) = text $ show s

--- pretty-print a record pattern
ppCFieldPattern :: Options -> CField CPattern -> Doc
ppCFieldPattern opts (qn, p) = ppQName opts qn <+> equals <+> ppCPattern opts p

--- pretty-print the right hand side of a rule (or case expression), including
--- the d sign, where `d` is the relation (as doc) between the left hand side
--- and the right hand side -- usually this is one of `=`, `->`.
ppCRhs :: Options -> Doc -> CRhs -> Doc
ppCRhs opts d (CSimpleRhs  exp lDecls)
    = d <+> ppCExpr opts exp
   $$ if null lDecls
         then empty
         else indent' opts (ppWhereDecl opts lDecls)
ppCRhs opts d (CGuardedRhs conds lDecls)
    = ppCGuardedRhs opts d conds
   $$ if null lDecls
         then empty
         else indent' opts (ppWhereDecl opts lDecls)

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
ppCLocalDecls opts d = (d <+>) . align . vcatMap (ppCLocalDecl opts)

--- pretty-print
ppWhereDecl :: Options -> [CLocalDecl] -> Doc
ppWhereDecl opts = (where_ <$>)
                 . indent' opts
                 . vcatMap (ppCLocalDecl opts)

ppLetDecl :: Options -> [CLocalDecl] -> Doc
ppLetDecl opts = (text "let" <+>) . align . vcatMap (ppCLocalDecl opts)

--- pretty-print local declarations (the part that follows the `where` keyword).
ppCLocalDecl :: Options -> CLocalDecl -> Doc
ppCLocalDecl opts (CLocalFunc fDecl) = ppCFuncDecl opts fDecl
ppCLocalDecl opts (CLocalPat  p rhs) = ppCPattern opts p
                                   <+> ppCRhs opts equals rhs
ppCLocalDecl opts (CLocalVars pvars)
    = (<+> text "free") $ hsep $ punctuate comma $ map (ppCVarIName opts) pvars

--- pretty-print an expression.
ppCExpr :: Options -> CExpr -> Doc
ppCExpr = ppCExpr' tlPrec

-- Internal use only: Pretty-print an expression and make use of supplied
-- precedence context. The supplied number represents the precedence of the
-- enclosing expression. Higher values mean more precedence, so if the nested
-- expression has lower precedence than the enclosing expression, the nested one
-- has to be enclosed in parentheses.
ppCExpr' :: Int -> Options -> CExpr -> Doc
ppCExpr' _ opts (CVar     pvar) = ppCVarIName opts pvar
ppCExpr' _ opts (CLit     lit ) = ppCLiteral opts lit
ppCExpr' _ opts (CSymbol  qn  ) = parensIf (isInfixId qn) $ ppQName opts qn -- TODO: Is this too greedy?
ppCExpr' p opts app@(CApply f exp)
    | isITE app = parensIf (p > tlPrec)
                $ let (c, t, e) = fromJust $ extractITE app
                  in  text "if" <+> group (align $ ppCExpr opts c
                                                $$ text "then" <+> ppCExpr opts t
                                                $$ text "else" <+> ppCExpr opts e)
    | isInf app = parensIf (p >= infAppPrec)
                $ let (op, l, r) = fromJust $ extractInfix app
                  in  ppCExpr' infAppPrec opts l
                  <+> ppQName opts op
                  <+> ppCExpr' infAppPrec opts r
    | isTup app = let args = fromJust $ extractTuple app
                  in  hsep $ lparen
                           : punctuate comma (map (ppCExpr opts) args)
                          ++ [rparen]
    | otherwise = parensIf (p >= prefAppPrec)
                $ ppCExpr opts f <+> ppCExpr' prefAppPrec opts exp
    where isITE = isJust . extractITE
          isInf = isJust . extractInfix
          isTup = isJust . extractTuple
ppCExpr' p opts (CLambda ps exp) = parensIf (p > tlPrec)
                                 $ backslash
                                <> hsepMap (ppCPattern' prefAppPrec opts) ps
                               <+> arrow
                               <+> ppCExpr opts exp
ppCExpr' p opts (CLetDecl lDecls exp) = parensIf (p > tlPrec)
                                      $ group
                                      $ align
                                      $ ppLetDecl opts lDecls
                                    <$> text "in"
                                    <+> ppCExpr opts exp
ppCExpr' p opts (CDoExpr stms) = parensIf (p > tlPrec)
                               $ text "do"
                             <+> align (vcatMap (ppCStatement opts) stms)
ppCExpr' _ opts (CListComp exp stms) = brackets
                                     $ ppCExpr opts exp
                                   <+> bar
                                   <+> fillSep (punctuate (comma <> space)
                                                          (map (ppCStatement opts)
                                                               stms))
ppCExpr' p opts (CCase cType exp cases) = parensIf (p > tlPrec)
                                        $ ppCCaseType cType
                                      <+> ppCExpr opts exp
                                      <+> text "of"
                                      <+> ppCases opts cases
ppCExpr' p opts (CTyped exp tExp) = parensIf (p > tlPrec)
                                  $ ppCExpr opts exp
                                <+> doubleColon
                                <+> ppCTypeExpr opts tExp
ppCExpr' _ opts (CRecConstr qn rFields) = ppQName opts qn
                                      <+> ppRecordFields opts rFields
ppCExpr' p opts (CRecUpdate exp rFields) = ppCExpr' p opts exp
                                       <+> ppRecordFields opts rFields

ppCStatement :: Options -> CStatement -> Doc
ppCStatement opts (CSExpr exp       ) = ppCExpr opts exp
ppCStatement opts (CSPat  pat    exp) = ppCPattern opts pat
                                    <+> larrow
                                    <+> ppCExpr opts exp
ppCStatement opts (CSLet  lDecls    ) = ppLetDecl opts lDecls

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

          filteredTs = filter (\t -> case t of
                                          (CType    _ Public _ _) -> True
                                          (CTypeSyn _ Public _ _) -> True
                                          (CNewType _ Public _ _) -> True
                                          _                       -> False)
                              ts
          filteredFs = filter (\f -> case f of
                                          (CFunc     _ _ Public _ _) -> True
                                          (CmtFunc _ _ _ Public _ _) -> True
                                          _                          -> False)
                              fs
          ppQName' qn = parensIf (isInfixId qn) $ ppQName opts qn

--- pretty-print a QName qualified according to given options.
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

--- pretty-print a QName non-qualified.
ppName :: QName -> Doc
ppName = text . snd

-- Helping functions (sugaring)
--- `positionIdent f qn [x1, ..., xn]` will return `[x1, f qn, xn]` if `qn`
--- is an infix identifier and n = 2. If `qn` in the tuple identifier return
--- (simplified) `[(x1,, ..., xn)]`. Otherwise return `[f qn, x1, ..., xn]`.
positionIdent :: (QName -> Doc) -> QName -> [Doc] -> [Doc]
positionIdent f qn ds
    | null ds        = [prefixQnDoc]
    | isInfixId qn   = case ds of [x1, x2] -> [x1, qnDoc, x2]
                                  _        -> prefixQnDoc:ds
    | isTupleCons qn = tupledDs
    | otherwise      = prefixQnDoc:ds
    where prefixQnDoc = parensIf (isInfixId qn) qnDoc
          qnDoc       = f qn
          tupledDs = lparen : punctuate comma ds ++ [rparen]

-- Helping function (diagnosis)
--- Check whether an operator is an infix identifier.
isInfixId :: QName -> Bool
isInfixId = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

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

--- Check if given application tree represents an if then else construct.
--- If so, return the condition, the "then expression" and the "else expression".
--- Otherwise, return `Nothing`.
extractITE :: CExpr -> Maybe (CExpr, CExpr, CExpr)
extractITE e = case e of
                    (CApply (CApply (CApply (CSymbol ("Prelude","if_then_else"))
                                            cond)
                                    tExp)
                            fExp) -> Just (cond, tExp, fExp)
                    _             -> Nothing

--- Check if given application tree represents an infix operator application.
--- If so, return the operator, its left and its right argument. Otherwise,
--- return `Nothing`.
extractInfix :: CExpr -> Maybe (QName, CExpr, CExpr)
extractInfix e = case e of
                      (CApply (CApply (CSymbol s)
                                      e1)
                              e2) | isInfixId s -> Just (s, e1, e2)
                      _                         -> Nothing

--- Check if given application tree represents a tuple contructor application.
--- If so, return the constructor and its arguments in a list. Otherwise, return
--- `Nothing`.
extractTuple :: CExpr -> Maybe [CExpr]
extractTuple = extractTuple' []
    where extractTuple' es exp = case exp of
                (CApply  f e)                 -> extractTuple' (e:es) f
                (CSymbol s  ) | isTupleCons s -> Just es
                _                             -> Nothing

-- Helping functions (pretty printing)
vsepBlankMap :: (a -> Doc) -> [a] -> Doc
vsepBlankMap f = vsepBlank . map f

indent' :: Options -> Doc -> Doc
indent' opts = indent (indentationWidth opts)

bquotesIf :: Bool -> Doc -> Doc
bquotesIf b d = if b then bquotes d else d

-- later replaced by library version
infixl 1 <++>
(<++>) :: Doc -> Doc -> Doc
l <++> r
    | l == empty = r
    | r == empty = l
    | otherwise  = l <+> r

larrow :: Doc
larrow = text "<-"

where_ :: Doc
where_ = text "where"

nil :: Doc
nil = text "[]"