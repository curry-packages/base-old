--- --------------------------------------------------------------------------
--- Pretty printing of AbstractCurry.
---
--- This library provides a pretty-printer for AbstractCurry modules.
---
--- @author  Yannik Potdevin
--- @version August 2015
--- --------------------------------------------------------------------------
module AbstractCurry.Pretty where

import Pretty
import AbstractCurry
import List (partition)
import Maybe (isJust, fromJust)

data Qualification
    = Full      -- ^ Fully qualify every function, including those of the
                --   processed module and Prelude
    | Imports   -- ^ Fully qualify external functions, do not qualify local
                --   functions and those of Prelude
    | None      -- ^ Do not qualify any function

data LayoutChoice = PreferNestedLayout  -- ^ Prefer
                                        -- a                      f a
                                        -- + b      respectively    b
                                        --   + ...                  c
                                        -- if an expression does not fit the page
                  | PreferFilledLayout  -- ^ Prefer
                                        -- a + b                  f a b
                                        -- + c + d  respectively  c d
                                        -- if an expression does not fit the page

data Options = Options { pageWidth        :: Int
                       , indentationWidth :: Int
                       , qualification    :: Qualification
                       , moduleName       :: String
                       , showLocalSigs    :: Bool -- debugging flag (show signature of local functions or not).
                       , layoutChoice     :: LayoutChoice }

options :: Int              -- ^ page width
        -> Int              -- ^ indentation width
        -> Qualification    -- ^ what names to qualify
        -> MName            -- ^ the name of current module
        -> Options
options pw iw q m = Options { pageWidth        = pw
                            , indentationWidth = iw
                            , qualification    = q
                            , moduleName       = m
                            , showLocalSigs    = False
                            , layoutChoice     = PreferNestedLayout }

defaultOptions :: Options
defaultOptions = options 80 4 Imports ""

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
     = ( group . nest' opts'
       $ text "module" <+> ppMName m $$ ppExports opts' ts fs) </> where_
 <$+$> ppImports opts' ms
 <$+$> vcatMap (ppCOpDecl opts') os
 <$+$> vcatMap (ppCTypeDecl opts') ts
 <$+$> vsepBlankMap (ppCFuncDecl opts') fs
    where opts' = opts { moduleName = m }

--- pretty-print a module name (just a string).
ppMName :: MName -> Doc
ppMName = text

--- pretty-print exports, i.e. all type  and function declarations which are
--- public.
--- extract the type and function declarations which are public and gather their
--- qualified names in a list.
ppExports :: Options -> [CTypeDecl] -> [CFuncDecl] -> Doc
ppExports opts ts fs
    | null pubTs  && null pubFs  = parens empty -- nothing is exported
    | null privTs && null privFs = empty        -- everything is exported
    | otherwise                  = tupledSpaced $ map tDeclToDoc pubTs
                                               ++ map fDeclToDoc pubFs
    where (pubTs, privTs) = partition isPublicTypeDecl ts
          (pubFs, privFs) = partition isPublicFuncDecl fs

          isPublicTypeDecl tDecl = case tDecl of
                                        (CType    _ Public _ _) -> True
                                        (CTypeSyn _ Public _ _) -> True
                                        (CNewType _ Public _ _) -> True
                                        _                       -> False

          isPublicFuncDecl fDecl = case fDecl of
                                        (CFunc     _ _ Public _ _) -> True
                                        (CmtFunc _ _ _ Public _ _) -> True
                                        _                          -> False

          tDeclToDoc (CType    qn _ _ cDecls)
              = ppQName' qn <> ppConsExports opts cDecls
          tDeclToDoc (CTypeSyn qn _ _ _     ) = ppQName' qn
          tDeclToDoc (CNewType qn _ _ cDecl )
              = ppQName' qn <> ppConsExports opts [cDecl]

          fDeclToDoc (CFunc     qn _ _ _ _) = ppQName' qn
          fDeclToDoc (CmtFunc _ qn _ _ _ _) = ppQName' qn

          ppQName' = genericPPQName parsIfInfix opts

-- internal use only
ppConsExports :: Options -> [CConsDecl] -> Doc
ppConsExports opts cDecls
    | null pubCs  = empty
    | null privCs = parens $ dot <> dot
    | otherwise   = tupledSpaced $ map cDeclToDoc pubCs
    where (pubCs, privCs)    = partition isPublicConsDecl cDecls
          isPublicConsDecl cDecl
              = case cDecl of
                     (CCons   _ Public _) -> True
                     (CRecord _ Public _) -> True
                     _                    -> False

          cDeclToDoc (CCons   qn _ _) = ppQName' qn
          cDeclToDoc (CRecord qn _ _) = ppQName' qn

          ppQName' = genericPPQName parsIfInfix opts


--- pretty-print imports (list of module names) by prepending the word "import"
--- to the module name.
ppImports :: Options -> [MName] -> Doc
ppImports _ = vcatMap (\m -> text "import" <+> ppMName m)

--- pretty-print operator precedence declarations.
ppCOpDecl :: Options -> COpDecl -> Doc
ppCOpDecl _ (COp qn fix p) = ppCFixity fix
                         <+> int p
                         <+> genericPPName (bquotesIf . not . isInfixId) qn

--- pretty-print the fixity of a function.
ppCFixity :: CFixity -> Doc
ppCFixity CInfixOp  = text "infix"
ppCFixity CInfixlOp = text "infixl"
ppCFixity CInfixrOp = text "infixr"

--- pretty-print type declarations, like `data ... = ...`, `type ... = ...` or
--- `newtype ... = ...`.
ppCTypeDecl :: Options -> CTypeDecl -> Doc
ppCTypeDecl opts (CType qn _ tVars cDecls)
    = hsep [ text "data", ppName qn, ppCTVarINames opts tVars
           , if null cDecls then empty else ppCConsDecls opts cDecls]
ppCTypeDecl opts (CTypeSyn qn _ tVars tExp)
    = hsep [ text "type", ppName qn, ppCTVarINames opts tVars
           , align $ equals <+> ppCTypeExpr opts tExp]
ppCTypeDecl opts (CNewType qn _ tVars cDecl)
    = hsep [ text "newtype", ppName qn, ppCTVarINames opts tVars, equals
           , ppCConsDecl opts cDecl]

--- pretty-print a list of constructor declarations, including the `=` sign.
ppCConsDecls :: Options -> [CConsDecl] -> Doc
ppCConsDecls opts cDecls
    = align . sep $ [equals <+> ppCConsDecl opts (head cDecls)]
                    ++ map ((bar <+>) . (ppCConsDecl opts)) (tail cDecls)

--- pretty-print a constructor declaration.
ppCConsDecl :: Options -> CConsDecl -> Doc
ppCConsDecl opts (CCons   qn _ tExps ) = ppName qn
                                    <++> hsepMap (ppCTypeExpr' 2 opts) tExps
ppCConsDecl opts (CRecord qn _ fDecls) = ppName qn
                                     <+> setSpaced (map (ppCFieldDecl opts)
                                                        fDecls)

--- pretty-print a record field declaration (`field :: type`).
ppCFieldDecl :: Options -> CFieldDecl -> Doc
ppCFieldDecl opts (CField qn _ tExp) = hsep [ ppName qn
                                            , doubleColon
                                            , ppCTypeExpr opts tExp ]

--- pretty-print a function declaration.
ppCFuncDecl :: Options -> CFuncDecl -> Doc
ppCFuncDecl opts fDecl@(CFunc qn _ _ tExp _)
    = ppCFuncSignature opts qn tExp <$!$> ppCFuncDeclWithoutSig opts fDecl
ppCFuncDecl opts (CmtFunc cmt qn a v tExp rs)
    = string cmt <$!$> ppCFuncDecl opts (CFunc qn a v tExp rs)

--- pretty-print a function declaration without signature.
ppCFuncDeclWithoutSig :: Options -> CFuncDecl -> Doc
ppCFuncDeclWithoutSig opts (CFunc qn _ _ _ rs)
    = ppCRules opts qn rs
ppCFuncDeclWithoutSig opts (CmtFunc cmt qn a v tExp rs)
    = string cmt <$!$> ppCFuncDeclWithoutSig opts (CFunc qn a v tExp rs)

--- pretty-print a function signature according to given options.
ppCFuncSignature :: Options -> QName -> CTypeExpr -> Doc
ppCFuncSignature opts qn tExp
    = nest' opts
    $ sep [ genericPPName parsIfInfix qn
          , align (doubleColon <+> ppCTypeExpr opts tExp)]

--- pretty-print a type expression.
ppCTypeExpr :: Options -> CTypeExpr -> Doc
ppCTypeExpr = ppCTypeExpr' tlPrec

-- Internal use only: Pretty-print a type expression and make use of supplied
-- precedence context. The supplied number represents the precedence of the
-- enclosing expression. Higher values mean more precedence, so if the nested
-- expression has lower precedence than the enclosing expression, the nested one
-- has to be enclosed in parentheses.
ppCTypeExpr' :: Int -> Options -> CTypeExpr -> Doc
ppCTypeExpr' _ opts (CTVar     tvar) = ppCTVarIName opts tvar
ppCTypeExpr' p opts (CFuncType tExp1 tExp2)
    = parensIf (p > tlPrec)
    $ sep [ ppCTypeExpr' 1 opts tExp1, rarrow <+> ppCTypeExpr opts tExp2]
ppCTypeExpr' p opts (CTCons qn tExps)
    | null tExps     = ppQName opts qn
    | isListCons qn  = brackets . (ppCTypeExpr opts) . head $ tExps -- assume singleton
    | isTupleCons qn = tupledSpaced $ map (ppCTypeExpr opts) tExps
    | otherwise      = parensIf (p >= 2)
                     $ ppQName opts qn <+> hsepMap (ppCTypeExpr' 2 opts) tExps

--- pretty-print a list of type variables horizontally separating them by `space`.
ppCTVarINames :: Options -> [CTVarIName] -> Doc
ppCTVarINames opts = hsepMap (ppCTVarIName opts)

--- pretty-print a type variable (currently the Int is ignored).
ppCTVarIName :: Options -> CTVarIName -> Doc
ppCTVarIName _ (_, tvar) = text tvar

--- pretty-print a list of function rules, concatenated vertically.
ppCRules :: Options -> QName -> [CRule] -> Doc
ppCRules opts qn rs = case rs of
                           [] -> genericPPName parsIfInfix qn
                             <+> text "external"
                           _  -> vcatMap (ppCRule opts qn) rs

--- pretty-print a rule of a function. Given a function
--- `f x y = x * y`, then `x y = x * y` is a rule consisting of `x y` as list of
--- patterns and `x * y` as right hand side.
ppCRule :: Options -> QName -> CRule -> Doc
ppCRule opts qn (CRule ps rhs)
    = (group . nest' opts $ hsep (positionIdent ppName qn pDocs)
                         $$ pRhs)
   $$ if null lDecls
         then empty
         else indent' opts $ ppWhereDecl opts lDecls
    where pDocs          = map (ppCPattern' prefAppPrec opts) ps
          (pRhs, lDecls) = ppCRhsWithoutLocalDecls opts equals rhs

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
                                  $ hsep [ ppCPattern' infAppPrec opts p1
                                         , ppQName opts qn
                                         , ppCPattern' infAppPrec opts p2 ]
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
                   | isInfixId qn  -> parensIf (p >= infAppPrec)
                                    $ hsep [l, m, r]
         {- Assume tupled pattern and therefore avoid additional parenthesis. -}
         (x:_) | x == lparen       -> hsep pDocs
         {- Assume prefix application. -}
         _                         -> parensIf (p >= prefAppPrec)
                                    . group . nest' opts
                                    $ head pDocs $$ (align .sep $ tail pDocs)
    where pDocs = positionIdent (ppQName opts) qn
                $ map (ppCPattern' p' opts) ps
          p'    = if isInfixId qn
                     then infAppPrec
                     {- This assumes the existence of infix constructors and
                        all of them having a lower precedence than any prefix
                        constructors. -}
                     else prefAppPrec


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
--- If the right hand side contains local declarations, they will be pretty
--- printed too, further indented.
ppCRhsWithLocalDecls :: Options -> Doc -> CRhs -> Doc
ppCRhsWithLocalDecls opts d (CSimpleRhs  exp lDecls)
    = (group . nest' opts $ d $$ ppCExpr opts exp)
   $$ if null lDecls
         then empty
         else indent' opts (ppWhereDecl opts lDecls)
ppCRhsWithLocalDecls opts d (CGuardedRhs conds lDecls)
    = ppCGuardedRhs opts d conds
   $$ if null lDecls
         then empty
         else indent' opts (ppWhereDecl opts lDecls)

--- Like `ppCRhsWithLocalDecls`, but do not pretty print local declarations.
--- Instead give caller the choice how to handle the declarations. for example
--- For example the function `ppCRule` uses this to prevent local declarations
--- from being further indented.
ppCRhsWithoutLocalDecls :: Options -> Doc -> CRhs -> (Doc, [CLocalDecl])
ppCRhsWithoutLocalDecls opts d (CSimpleRhs  exp lDecls)
    = (d <+> ppCExpr opts exp, lDecls)
ppCRhsWithoutLocalDecls opts d (CGuardedRhs conds lDecls)
    = (ppCGuardedRhs opts d conds, lDecls)

--- pretty-print guard, i.e. the `| cond d exp` part of a right hand side, where
--- `d` is the relation (as doc) between `cond` and `exp` -- usually this is
--- one of `=`, `->`.
ppCGuardedRhs :: Options -> Doc -> [(CExpr, CExpr)] -> Doc
ppCGuardedRhs opts d = align . vvsepMap ppCGuard
    where ppCGuard (e1, e2) = bar <+> (group . nest' opts
                                     $ ppCExpr opts e1
                                    $$ d <+> ppCExpr opts e2)

--- pretty-print local declarations . If the second argument is `text "where"`,
--- pretty-print a `where` block. If the second argument is `text "let"`,
--- pretty-print a `let` block without `in`.
ppCLocalDecls :: Options -> Doc -> [CLocalDecl] -> Doc
ppCLocalDecls opts d = (d <+>) . align . vvsepMap (ppCLocalDecl opts)

--- pretty-print local declarations (the part that follows the `where` keyword).
ppCLocalDecl :: Options -> CLocalDecl -> Doc
ppCLocalDecl opts (CLocalFunc fDecl)
    = if showLocalSigs opts
         then ppCFuncDecl opts fDecl
         else ppCFuncDeclWithoutSig opts fDecl
ppCLocalDecl opts (CLocalPat  p rhs)
    = hsep [ ppCPattern opts p, ppCRhsWithLocalDecls opts equals rhs ]
ppCLocalDecl opts (CLocalVars pvars)
    = (<+> text "free") $ hsep $ punctuate comma $ map (ppCVarIName opts) pvars

--- pretty-print a `where` block, where `where` is above following declarations.
ppWhereDecl :: Options -> [CLocalDecl] -> Doc
ppWhereDecl opts = (where_ $$)
                 . indent' opts
                 . vvsepMap (ppCLocalDecl opts)

--- pretty-print a `let` block without `in`.
ppLetDecl :: Options -> [CLocalDecl] -> Doc
ppLetDecl opts = ppCLocalDecls opts (text "let")

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
ppCExpr' _ opts (CSymbol  qn  ) = genericPPQName parsIfInfix opts qn
ppCExpr' p opts app@(CApply f exp)
    | isITE app
        = parensIf (p > tlPrec)
        $ let (c, t, e) = fromJust $ extractITE app
          in  text "if" <+> (align $ sep [ ppCExpr opts c
                                         , text "then" <+> ppCExpr opts t
                                         , text "else" <+> ppCExpr opts e])
    | isTup app = let args = fromJust $ extractTuple app
                  in  tupledSpaced (map (ppCExpr opts) args)
    | isFinLis app = let elems = fromJust $ extractFiniteList app
                     in  listSpaced (map (ppCExpr opts) elems)
    | isInf app
        = parensIf (p >= infAppPrec)
        $ let (op, l, r) = fromJust $ extractInfix app
          in  (case layoutChoice opts of
                    PreferNestedLayout -> ppNestedWay
                    PreferFilledLayout -> ppFilledWay)
                        (ppCExpr' infAppPrec opts l)
                        (ppQName opts op)
                        (ppCExpr' infAppPrec opts r)
    | otherwise = parensIf (p >= prefAppPrec)
                $ (case layoutChoice opts of
                        PreferNestedLayout -> ppNestedWay
                        PreferFilledLayout -> ppFilledWay)
                            (ppCExpr' infAppPrec opts f)
                            empty
                            (ppCExpr' prefAppPrec opts exp)
    where isITE    = isJust . extractITE
          isInf    = isJust . extractInfix
          isTup    = isJust . extractTuple
          isFinLis = isJust . extractFiniteList
--           ppNestedWay l sep r = group . align . nest 1 $ l $$ (sep <++> r)
          ppNestedWay l sepa r = nest 1 $ sep [l, sepa <++> r]
          ppFilledWay l sepa r = nest 1 $ fillSep [l, sepa, r]
ppCExpr' p opts (CLambda ps exp)
    = parensIf (p > tlPrec) . nest' opts
    $ sep [ backslash <> hsepMap (ppCPattern' prefAppPrec opts) ps
                     <+> rarrow
          , ppCExpr opts exp]
ppCExpr' p opts (CLetDecl lDecls exp) = parensIf (p > tlPrec) . group . align
                                      $ ppLetDecl opts lDecls
                                     $$ text "in"
                                    <+> ppCExpr opts exp
ppCExpr' p opts (CDoExpr stms) = parensIf (p > tlPrec)
                               $ text "do"
                             <+> align (vvsepMap (ppCStatement opts) stms)
ppCExpr' _ opts (CListComp exp stms)
    = brackets $ ppCExpr opts exp
             <+> bar
             <+> hsep (punctuate (comma <> space) (map (ppCStatement opts)
                                                        stms))
ppCExpr' p opts (CCase cType exp cases)
    = parensIf (p > tlPrec) . group $ nest' opts ( ppCCaseType cType
                                               <+> ppCExpr opts exp
                                               <+> text "of"
                                                $$ ppCases opts cases)
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
ppCases opts = align . vvsepMap (ppCase opts)

--- pretty-print a case expression.
ppCase :: Options -> (CPattern, CRhs) -> Doc
ppCase opts (p, rhs) = ppCPattern opts p <+> ppCRhsWithLocalDecls opts rarrow rhs

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

-- pretty-print a QName qualified according to given options. Use given doc
-- tranformer to manipulate (f.e. surround with parentheses) the QName, after
-- it was (maybe) qualified.
genericPPQName :: (QName -> Doc -> Doc) -> Options -> QName -> Doc
genericPPQName g opts qn@(m, f)
    | qnIsBuiltIn       = preparedName
    | null m            = preparedName -- assume local declaration
    | otherwise         =
        case qualification opts of
             Full    -> preparedFQName
             Imports -> if m == moduleName opts || m == "Prelude"
                           then preparedName
                           else preparedFQName
             None    -> preparedName
    where qnIsBuiltIn    = or (map ($ qn) [ isUnitCons , isListCons
                                          , isTupleCons, isConsCons ])
          preparedFQName = g qn $ ppMName m <> dot <> text f
          preparedName   = g qn (text f)

--- pretty-print a QName qualified according to given options.
ppQName :: Options -> QName -> Doc
ppQName = genericPPQName (flip const)

genericPPName :: (QName -> Doc -> Doc) -> QName -> Doc
genericPPName f qn = f qn $ text . snd $ qn

--- pretty-print a QName non-qualified.
ppName :: QName -> Doc
ppName = genericPPName (flip const)

-- Helping functions (sugaring)
--- `positionIdent f qn [x1, ..., xn]` will return `[x1, f qn, xn]` if `qn`
--- is an infix identifier and n = 2. If `qn` in the tuple identifier return
--- (simplified) `[(x1,, ..., xn)]`. Otherwise return `[f qn, x1, ..., xn]`.
positionIdent :: (QName -> Doc) -> QName -> [Doc] -> [Doc]
positionIdent f qn ds
    | null ds        = [prefixQnDoc]
    | isInfixId qn   = case ds of [x1, x2] -> [x1, qnDoc, x2]
                                  _        -> prefixQnDoc:ds
    | isTupleCons qn = lparen : punctuate comma ds ++ [rparen]
    | otherwise      = prefixQnDoc:ds
    where prefixQnDoc = parensIf (isInfixId qn) qnDoc
          qnDoc       = f qn

-- Helping function (diagnosis)
--- Check whether an operator is an infix identifier.
isInfixId :: QName -> Bool
isInfixId = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents the unit constructor
isUnitCons :: QName -> Bool
isUnitCons (_, i) = i == "()"

--- Check whether an identifier represents the empty list constructor
isListCons :: QName -> Bool
isListCons (_, i) = i == "[]"

--- Check whether an identifier represents the list constructor `:`
isConsCons :: QName -> Bool
isConsCons (_, i) = i == ":"

--- Check whether an identifier represents a tuple constructor
isTupleCons :: QName -> Bool
isTupleCons (_, i) = i == mkTuple (length i)
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
extractInfix e
    = case e of (CApply (CApply (CSymbol s)
                                 e1)
                        e2)
                    | isInfixId s -> Just (s, e1, e2)
                _                 -> Nothing

--- Check if given application tree represents a tuple contructor application.
--- If so, return the constructor and its arguments in a list. Otherwise, return
--- `Nothing`.
extractTuple :: CExpr -> Maybe [CExpr]
extractTuple = extractTuple' []
    where extractTuple' es exp
            = case exp of
                   (CApply  f e)                 -> extractTuple' (e:es) f
                   (CSymbol s  ) | isTupleCons s -> Just es
                   _                             -> Nothing

--- Check if given application tree represents a finite list `[x1, ..., xn]`.
--- If so, return the list elements in a list. Otherwise, return `Nothing`.
extractFiniteList :: CExpr -> Maybe [CExpr]
extractFiniteList = extractFiniteList' []
    where extractFiniteList' es exp
            = case exp of
                   (CApply (CApply (CSymbol f)
                                    e)
                            arg) | isConsCons f -> extractFiniteList' (e:es) arg
                   (CSymbol s)   | isListCons s -> Just $ reverse es
                   _                            -> Nothing

-- Helping functions (pretty printing)
hsepMap :: (a -> Doc) -> [a] -> Doc
hsepMap f = hsep . map f

vcatMap :: (a -> Doc) -> [a] -> Doc
vcatMap f = vcat . map f

vsepMap :: (a -> Doc) -> [a] -> Doc
vsepMap f = vsep . map f

vsepBlankMap :: (a -> Doc) -> [a] -> Doc
vsepBlankMap f = vsepBlank . map f

vvsep :: [Doc] -> Doc
vvsep = compose (<$!$>)

vvsepMap :: (a -> Doc) -> [a] -> Doc
vvsepMap f = vvsep . map f

fillSepMap :: (a -> Doc) -> [a] -> Doc
fillSepMap f = fillSep . map f

nest' :: Options -> Doc -> Doc
nest' opts = nest (indentationWidth opts)

indent' :: Options -> Doc -> Doc
indent' opts = indent (indentationWidth opts)

bquotesIf :: Bool -> Doc -> Doc
bquotesIf b d = if b then bquotes d else d

parsIfInfix :: QName -> Doc -> Doc
parsIfInfix = parensIf . isInfixId

infixl 1 <++>
(<++>) :: Doc -> Doc -> Doc
l <++> r
    | isEmpty l = r
    | isEmpty r = l
    | otherwise = l <+> r

larrow :: Doc
larrow = text "<-"

where_ :: Doc
where_ = text "where"

nil :: Doc
nil = text "[]"