-------------------------------------------------------------------------------
--- This library contains a pretty printer for AbstractCurry programs
--- in order to show an AbstractCurry program in human-readable Curry syntax.
--- In contrast to the library `AbstractCurryPrinter`,
--- this library implements a better human-readable pretty printing
--- of AbstractCurry programs.
---
--- @author Stefan Junge
--- @version June 2011
--- @category meta
-------------------------------------------------------------------------------
module PrettyAbstract (showCProg, cprogDoc,
                       prettyCProg, cprogDocWithPrecedences,
                       preludePrecs, precs, prettyCTypeExpr,
                       prettyCTypes, prettyCOps) where

import Pretty
import AbstractCurry
import AbstractCurryGoodies ( typeCons, typeName, typeVis, funcName, funcVis
                            , consVis, argTypes, resultType)
import Char
import System
import Maybe
import List (groupBy)

--- Should names from imported modules be shown with module prefixes?
qualifiedNames :: Bool
qualifiedNames = True --False

debug :: Bool
debug = False

showPrecs :: a -> b -> Doc
showPrecs name prec
    | debug = text ("{-" ++ show name ++ "@" ++ show prec ++ "-}")
    | otherwise = empty

prelude :: String
prelude = "Prelude"

type Precs = [(QName,(CFixity,Int))]

--- the precedences of the operators in the <code>Prelude</code> module
--- @return a list of precedences
preludePrecs :: Precs
preludePrecs =
    [(("Prelude","!!"),(CInfixlOp,9)),(("Prelude","."),(CInfixrOp,9)),
     (("Prelude","mod"),(CInfixlOp,7)),(("Prelude","div"),(CInfixlOp,7)),
     (("Prelude","*"),(CInfixlOp,7)),(("Prelude","-"),(CInfixlOp,6)),
     (("Prelude","+"),(CInfixlOp,6)),(("Prelude","++"),(CInfixrOp,5)),
     (("Prelude","=:<<="),(CInfixOp,4)),(("Prelude","=:<="),(CInfixOp,4)),
     (("Prelude",">="),(CInfixOp,4)),(("Prelude","<="),(CInfixOp,4)),
     (("Prelude",">"),(CInfixOp,4)),(("Prelude","<"),(CInfixOp,4)),
     (("Prelude","/="),(CInfixOp,4)),(("Prelude","=="),(CInfixOp,4)),
     (("Prelude","=:="),(CInfixOp,4)),(("Prelude","notElem"),(CInfixOp,4)),
     (("Prelude","elem"),(CInfixOp,4)),(("Prelude","&&"),(CInfixrOp,3)),
     (("Prelude","||"),(CInfixrOp,2)),(("Prelude",">>="),(CInfixlOp,1)),
     (("Prelude",">>"),(CInfixlOp,1)),(("Prelude","?"),(CInfixrOp,0)),
     (("Prelude","&>"),(CInfixrOp,0)),(("Prelude","&"),(CInfixrOp,0)),
     (("Prelude","seq"),(CInfixrOp,0)),(("Prelude","$##"),(CInfixrOp,0)),
     (("Prelude","$#"),(CInfixrOp,0)),(("Prelude","$!!"),(CInfixrOp,0)),
     (("Prelude","$!"),(CInfixrOp,0)),(("Prelude","$"),(CInfixrOp,0)),
     (("Prelude",":"),(CInfixrOp,5))]


-- ----------------------------------------------------------------------------+

--- (prettyCProg w prog) pretty prints the curry prog <code>prog</code> and
--- fits it to a page width of <code>w</code> characters.
--- @param w - width of page
--- @param prog - a curry prog
--- @return a string, which represents the <code>prog</code>
prettyCProg :: Int -> CurryProg -> String
prettyCProg w = pretty w . cprogDoc

--- (prettyCTypeExpr mod typeExpr) pretty prints the type expression
--- <code>typeExpr</code> of the module <code>mod</code> and fits it to a page
--- width of 78 characters.
--- @param mod - module name of the current module
--- @param typeExpr - a type expression
--- @return a string, which represents the <code>typeExpr</code>
prettyCTypeExpr :: String -> CTypeExpr -> String
prettyCTypeExpr mod = pretty 78 . typeExprDoc mod 0

--- (prettyCTypes mod typeDecls) pretty prints the type declarations
--- <code>typeDecls</code> of the module <code>mod</code> and fits it to a page
--- width of 78 characters.
--- @param mod - module name of the current module
--- @param typeDecls - a list of type declarations
--- @return a string, which represents the <code>typeDecls</code>
prettyCTypes :: String -> [CTypeDecl] -> String
prettyCTypes mod = pretty 78 . typesDoc mod

--- (prettyCOps opDecls) pretty prints the operators
--- <code>opDecls</code> and fits it to a page width of 78 characters.
--- @param opDecls - a list of operators
--- @return a string, which represents the <code>opDecls</code>
prettyCOps :: [COpDecl] -> String
prettyCOps = pretty 78 . opsDoc

--- (showCProg prog) pretty prints the curry prog
--- <code>prog</code> and fits it to a page width of 78 characters.
--- @param prog - a curry prog
--- @return a string, which represents the <code>prog</code>
showCProg :: CurryProg -> String
showCProg = prettyCProg 78

--- (cprogDoc prog) creates a document of the Curry program
--- <code>prog</code> and fits it to a page width of 78 characters.
--- @param prog - a curry prog
--- @return the document, which represents the <code>prog</code>
cprogDoc :: CurryProg -> Doc
cprogDoc = cprogDocWithPrecedences preludePrecs

--- (cprogDocWithPrecedences precs prog) creates a document of the curry prog
--- <code>prog</code> and fits it to a page width of 78 characters,
--- the precedences <code>precs</code> ensure a correct bracketing
--- of infix operators
--- @param precs - a list of precedences
--- @param prog - a curry prog
--- @return the document, which represents the <code>prog</code>
cprogDocWithPrecedences :: Precs -> CurryProg -> Doc
cprogDocWithPrecedences ps cprog@(CurryProg name imps types funcs ops)
  = moduleHeaderDoc name cprog (exportedNames name cprog) <$>>
    impsDoc imps <$>> opsDoc ops <$>>
    typesDoc name types <$>>
    funcsDoc (precs ops ++ ps) name funcs <$> empty

--- generates a list of precedences
--- @param opDecls - a list of operators
--- @return a list of precedences
precs :: [COpDecl] -> Precs
precs = map (\(COp name fix i) -> (name,(fix,i)))

record :: Doc -> Doc
record doc | isEmpty doc = braces empty
           | otherwise   = braces $ space <> doc <> space

commaSepList :: [Doc] -> Doc
commaSepList = fillSep . punctuate comma

-- -------------------------------layout--------------------------------------

(<$>>) :: Doc -> Doc -> Doc
d1 <$>> d2 | isEmpty d1 = d2
           | isEmpty d2 = d1
           | otherwise = d1 <$> line <> d2

def :: Doc -> [CTVarIName] -> Doc -> Doc
def name params body = block (name <> paramDoc <$> body)
 where
  paramDoc = if null params then empty
              else space <> align (fillSep (map varDoc params))

block :: Doc -> Doc
block = group . hang 1

app :: Doc -> [Doc] -> Doc
app d ds = if null ds then d
            else block (fillEncloseSep empty empty space (d:ds))

par :: Maybe a -> Doc -> Doc
par mPrec = if isJust mPrec then parens else id

precFillEncloseSep :: Bool -> (CFixity,Int) -> Maybe (CFixity,Int) -> Doc -> Doc -> Doc -> [Doc] -> Doc
precFillEncloseSep amILeft p1 mp2 l r s ds
  | isNothing mp2 = fillEncloseSep empty empty s ds
  | otherwise = fillEncloseSep (pre p1 p2 l) (pre p1 p2 r) s ds
 where
  p2 = fromJust mp2

  pre (fO,pO) (fI,pI) br
   | pO>pI = empty
   | pO<pI = br
   | fO == CInfixOp = br
   | fO /= fI = br
   | amILeft && fI == CInfixrOp = br
   | not amILeft && fI == CInfixlOp = br
   | True  = empty

layout :: [Doc] -> Doc
layout = align . compose (combine (linesep "; "))

-- -------------------------------qname----------------------------------------

qname :: String -> QName -> Doc
qname prog mn@(mod,name)
  | mn == (prelude,"[]") || isTupleName mn = text name
  | isInfixName mn =
      if mod == prog || mod == prelude || not qualifiedNames
       then parens (text name)
       else parens (text mod <> dot <> (text name))
  | otherwise
    = if mod == prog || mod == prelude || not qualifiedNames
       then text name
       else text mod <> dot <> text name

isTupleName :: QName -> Bool
isTupleName (mod,name) = mod == prelude && elem (take 2 name) ["()","(,"]

isInfixName :: QName -> Bool
isInfixName (_,n) = all (`elem` infixIDs) n

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

varDoc :: CTVarIName  -> Doc
varDoc = text . snd

tvarDoc :: CTVarIName -> Doc
tvarDoc (i, v)
  | v /= "" && '_' /= head v = text v
  | v == "_"  = text v
  | i > 25    = text ('x' : show i)
  | otherwise = char (chr (97 + i))

litDoc :: CLiteral -> Doc
litDoc (CIntc    n) = int n
litDoc (CFloatc  x) = float x
litDoc (CCharc   c) = squotes (text (quoteChar c))
litDoc (CStringc s) = text (show s)

quoteChar :: Char -> String
quoteChar c = maybe [c] id (lookup c specialChars)

specialChars :: [(Char, String)]
specialChars = [('\\',"\\\\"),('\n',"\\n"),('\r',"\\r"),('\t',"\\t"),('"',"\\\"")]

-- -------------------------------module header and exports-------------------

moduleHeaderDoc :: String -> CurryProg -> [Doc] -> Doc
moduleHeaderDoc name cprog exports
    | hasPrivate cprog = text "module" <+> text name <+> exportsDoc exports <+>
                         text "where"
    | otherwise = text "module" <+> text name <+> text "where"

exportsDoc :: [Doc] -> Doc
exportsDoc xs
  = group (nest 1 (lparen <$> align (fillSep (punctuate comma xs)) <$> rparen))

hasPrivate :: CurryProg -> Bool
hasPrivate (CurryProg _ _ types funcs _) =
    any (Private==) (map typeVis types ++ map funcVis funcs)

exportedNames :: String -> CurryProg -> [Doc]
exportedNames mod (CurryProg _ _ types funcs _)
  = map typeExpDoc (filter ((Public==) . typeVis) types)
 ++ map (qname mod . funcName) (filter ((Public==) . funcVis) funcs)
 where
  typeExpDoc tdecl =
    let ecs = filter ((Public==) . consVis)
                     (typeCons tdecl)
     in qname mod (typeName tdecl) <> if null ecs then empty else text "(..)"

-- -------------------------------imports and ops------------------------------

impsDoc :: [String] -> Doc
impsDoc imps = vcat (map ((text "import" <+>) . text)
                         (filter (/=prelude) imps))

opsDoc :: [COpDecl] -> Doc
opsDoc ops = vcat (map opLineDoc (groupBy eqCOpDecl ops))

opLineDoc :: [COpDecl] -> Doc
opLineDoc [] = empty
opLineDoc ops@(COp _ fix prec:_) =
    text "infix" <> fixDoc fix <+> int prec <+> align (hsep (punctuate comma (map opDoc ops)))
  where
    fixDoc CInfixOp = empty
    fixDoc CInfixlOp = text "l"
    fixDoc CInfixrOp = text "r"

eqCOpDecl :: COpDecl -> COpDecl -> Bool
eqCOpDecl (COp _ fix1 prec1) (COp _ fix2 prec2) =
    fix1 == fix2 && prec1 == prec2

opDoc :: COpDecl -> Doc
opDoc (COp n@(_,name) _ _)
  = text infname
 where
  infname = if isInfixName n then name else '`':name++"`"

-- -------------------------------types----------------------------------------

typesDoc :: String -> [CTypeDecl] -> Doc
typesDoc mod = vcat . map (typeDoc mod)

typeDoc :: String -> CTypeDecl -> Doc
typeDoc mod (CType name _ params cs)
  = def (text "data" <+> qname mod name) params (consDeclsDoc mod cs)
typeDoc mod (CTypeSyn name _ params syn)
  = def (text "type" <+> qname mod name) params
        (equals <+> typeExprDoc mod 0 syn)
typeDoc mod (CNewType name _ params c)
  = def (text "newtype" <+> qname mod name) params (consDeclsDoc mod [c])

consDeclsDoc :: String -> [CConsDecl] -> Doc
consDeclsDoc mod consDecls
    | null consDecls = empty
    | otherwise = fillEncloseSep (equals<>space) empty (bar<>space)
                  $ map ((<>space) . consDeclDoc mod) consDecls

consDeclDoc :: String -> CConsDecl -> Doc
consDeclDoc mod (CCons name _ args)
  = app (qname mod name) (map (typeExprDoc mod 2) args)
consDeclDoc mod (CRecord name _ fieldDecls)
  = (qname mod name) <+> record (commaSepList (map (fieldDeclsDoc mod) fieldDecls))

fieldDeclsDoc :: String -> CFieldDecl -> Doc
fieldDeclsDoc mod (CField (_, name) _ ty)
  = text name <+> doubleColon <+> typeExprDoc mod 0 ty

-- p == 0 -> parent is list / tuple / this is function result
-- p == 1 -> parent is CFuncType
-- p == 2 -> parent is CTCons
typeExprDoc :: String -> Int -> CTypeExpr -> Doc
typeExprDoc _ _ (CTVar n) = tvarDoc n
typeExprDoc mod p (CTCons name args)
  | null args = qname mod name
  | name == (prelude,"[]") = brackets (typeExprDoc mod 0 (head args))
  | isTupleName name = tupled (map (typeExprDoc mod 0) args)
  | otherwise
    = (if p == 2 then parens else id) $ app (qname mod name) (map (typeExprDoc mod 2) args)
typeExprDoc mod p typ@(CFuncType _ _)
  = (if p > 0 then parens else id) $ fillEncloseSep empty empty (space<>arrow<>space)
     (map (typeExprDoc mod 1) (argTypes typ) ++
      [typeExprDoc mod 0 (resultType typ)])

isUntyped :: CTypeExpr -> Bool
isUntyped typ =
    case typ of
        (CTCons ("Prelude","untyped") []) -> True
        _ -> False


-- -------------------------------function and localDecls----------------------

funcsDoc :: Precs -> String -> [CFuncDecl] -> Doc
funcsDoc pr mod funcs = vcat (punctuate line (map (funcDoc pr mod) funcs))

funcDoc :: Precs -> String -> CFuncDecl -> Doc
funcDoc pr mod (CFunc name _ _ typ rules) =
  (if hasRec typ then text "--" else empty) <>
  (if isUntyped typ then empty else funcTypeDeclDoc mod name typ <$> empty) <>
  vsep (map (ruleDoc pr mod name) rules)
funcDoc pr mod (CmtFunc cmt name ar vis typ rules) =
  vsep (map (\l->text ("--- "++l)) (lines cmt)) <$>
  funcDoc pr mod (CFunc name ar vis typ rules)

hasRec :: CTypeExpr -> Bool
hasRec (CTVar _) = False
hasRec (CFuncType t1 t2) = hasRec t1 || hasRec t2
hasRec (CTCons _ ts) = any hasRec ts
--hasRec (CRecordType _ _) = True

localDeclsDoc :: Precs -> String -> [CLocalDecl] -> Doc
localDeclsDoc pr mod lds
  | null lds  = empty
  | otherwise = line <>  text "where"
                     <+> align (vsep (punctuate line
                                        (map (localDeclDoc pr mod) lds)))

localDeclDoc :: Precs -> String -> CLocalDecl -> Doc
localDeclDoc pr mod (CLocalFunc    f) =  funcDoc pr mod f
localDeclDoc pr mod (CLocalPat p rhs) =  hang 1 $ patternDoc mod p
                                     <+> rhsDoc pr mod equals rhs
localDeclDoc _  _   (CLocalVars   vs) =  hang 1 $ commaSepList (map tvarDoc vs)
                                     <+> text "free"

-- -------------------------------function type--------------------------------

funcTypeDeclDoc :: String -> QName -> CTypeExpr -> Doc
funcTypeDeclDoc mod name typ
  = def (qname mod name) [] (funcTypeDoc mod (argTypes typ) (resultType typ))

funcTypeDoc :: String -> [CTypeExpr] -> CTypeExpr -> Doc
funcTypeDoc mod args res
  = fillEncloseSep doubleColon empty (space<>arrow)
     ((map ((space<>) . typeExprDoc mod 1) args) ++
      (map ((space<>) . typeExprDoc mod 1) [res]))

-- -------------------------------rules----------------------------------------

ruleDoc :: Precs -> String -> QName -> CRule -> Doc
ruleDoc pr mod name (CRule patterns crhs)
  = hang 2 (hang 2 (nameAndParam <$> align (rhsDoc pr mod equals crhs)))
  where
    nameAndParam =
        if isInfixName name && length patterns == 2
            then patternDoc mod (patterns!!0) <+> text (snd name) <+> patternDoc mod (patterns!!1)
            else qname mod name <> paramDoc

    paramDoc = if null patterns
                     then empty
                 else space <> patternsDoc mod patterns

rhsDoc :: Precs -> String -> Doc -> CRhs -> Doc
rhsDoc pr mod eqOrArrow (CSimpleRhs e locals)
  =  eqOrArrow <+> expDoc unknown pr Nothing mod e
  <> localDeclsDoc pr mod locals
rhsDoc pr mod eqOrArrow (CGuardedRhs guardedExprs locals)
  =  vcat (map (guardedExprDoc pr mod eqOrArrow) guardedExprs)
  <> localDeclsDoc pr mod locals

guardedExprDoc :: Precs -> String -> Doc -> (CExpr, CExpr) -> Doc
guardedExprDoc pr mod eqOrArrow (g, e)
  = bar <+> align (expDoc unknown pr Nothing mod g)
        <+> eqOrArrow
        <+> expDoc unknown pr Nothing mod e

-- -------------------------------expressions----------------------------------

expDoc :: Bool -> Precs -> Maybe (CFixity,Int) -> String -> CExpr -> Doc
expDoc amILeft pr mPrec mod exp =
  maybe (maybe (expDoc2 amILeft pr mPrec mod exp)
          (\l -> list (map (expDoc unknown pr Nothing mod) l))
          (toList exp))
    (\s -> if null s then text "[]" else dquotes (text s))
      (toString exp)


toList :: CExpr -> Maybe [CExpr]
toList exp
  = case exp of
      CSymbol ("Prelude","[]") -> Just []
      CApply (CApply (CSymbol ("Prelude",":")) x) xs -> toList xs >>- Just . (x:)
      _ -> Nothing

toString :: CExpr -> Maybe String
toString exp
  = case exp of
      CSymbol ("Prelude","[]") -> Just ""
      CApply (CApply (CSymbol ("Prelude",":")) (CLit (CCharc c))) cs ->
        toString cs >>- Just . (quoteChar c++)
      _ -> Nothing

expDoc2 :: Bool -> Precs -> Maybe (CFixity,Int) -> String -> CExpr -> Doc
expDoc2 _ _ mPrec _ (CVar n) = showPrecs n mPrec <> varDoc n
expDoc2 _ _ mPrec _ (CLit l) = showPrecs l mPrec <> litDoc l
expDoc2 _ _ mPrec mod (CSymbol qn) = showPrecs qn mPrec <> qname mod qn

expDoc2 amILeft pr mPrec mod (CApply e1 e2)
    | maybe False isTupleName mfname = tupled (map (expDoc unknown pr Nothing mod) fargs)
    | maybe False isInfixName mfname && length fargs == 2
       = showPrecs (snd fname) (amILeft,pOp, mPrec) <>
         (align $ precFillEncloseSep amILeft pOp mPrec lbr rbr empty
                 [expDoc True pr (Just pOp) mod (fargs!!0)
                 ,space <> text (snd fname)
                 ,space <> expDoc False pr (Just pOp) mod (fargs!!1)])
    | maybe False isInfixName mfname && length fargs > 2
       = appPar True mPrec $
         app (fillEncloseSep lparen rparen empty
                 [expDoc  True pr (Just pOp) mod (fargs!!0)
                 ,space <> text (snd fname)
                 ,space <> expDoc False pr (Just pOp) mod (fargs!!1)])
             (map (expDoc False pr (Just (unknown,11)) mod) (drop 2 fargs))
    | maybe False (== ("Prelude","if_then_else")) mfname && length fargs == 3 =
        par mPrec $ hang 1 $ ifThenElse
    | maybe False (== ("Prelude","if_then_else")) mfname && length fargs > 3 =
        appPar True mPrec $ hang 1 $
        app (parens ifThenElse)
            (map (expDoc False pr (Just (unknown,11)) mod) (drop 3 fargs))
    | otherwise = showPrecs (name (CApply e1 e2)) mPrec <> (appPar (not (null fargs)) mPrec $
                  app (expDoc False pr (Just (unknown,11)) mod (name (CApply e1 e2)))
                      (map (expDoc False pr (Just (unknown,11)) mod) fargs))

 where

    appPar _ Nothing = id
    appPar br (Just fNp)
        | snd fNp == 11 && br = parens
        | otherwise = id

    ifThenElse =
        text "if" <+> align (expDoc unknown pr Nothing mod (fargs!!0)) <$>
        text "then" <+> align (expDoc unknown pr Nothing mod (fargs!!1)) <$>
        text "else" <+> align (expDoc unknown pr Nothing mod (fargs!!2))

    fname = maybe ("","") id mfname

    mfname = case name (CApply e1 e2) of
                CSymbol qn -> Just qn
                _          -> Nothing
    fargs = args (CApply e1 e2)

    name e = case e of
        (CApply e' _) -> name e'
        _ -> e
    args e = case e of
        (CApply e1' e2') -> args e1' ++ [e2']
        _ -> []

    (lbr,rbr) = if isJust mPrec then (lparen,rparen) else (empty,empty)
    pOp = case lookup fname pr of
               Just p' -> p'
               Nothing -> (CInfixlOp,9)

expDoc2 _ pr mPrec mod (CLambda ps e)
  = par mPrec $ hang 1 $
     backslash  <+> patternsDoc mod ps
       <+> arrow <+> expDoc unknown pr Nothing mod e

expDoc2 _ pr mPrec mod (CLetDecl bs e)
  = par mPrec $ hang 1 $
     text "let" <+> localDeclsDoc pr mod bs <$>
     text "in" <+> expDoc unknown pr Nothing mod e

expDoc2 _ pr mPrec mod (CDoExpr stms)
  = par mPrec $
      text "do" <+> layout (map (statementDoc pr mod) stms)

expDoc2 _ pr _ mod (CListComp e stms)
  = brackets $ expDoc unknown pr Nothing mod e <+> text "|" <+>
               encloseSep empty empty comma (map (statementDoc pr mod) stms)

expDoc2 _ pr mPrec mod (CCase ct e bs)
  = par mPrec $ hang 1 $
     caseTypeDoc ct <+> align (expDoc unknown pr Nothing mod e)
       <+> text "of" <$> layout (map (branchDoc pr mod) bs)
  where
    caseTypeDoc CRigid = text "case"
    caseTypeDoc CFlex  = text "fcase"

expDoc2 _ pr mPrec mod (CTyped e ty)
  = par mPrec $
      expDoc unknown pr Nothing mod e <+> doubleColon <+> typeExprDoc mod 0 ty

expDoc2 _ pr mPrec mod (CRecConstr (_, name) cfields)
  = par mPrec $
      text name <+> record (commaSepList (map (fieldDoc (expDoc unknown pr Nothing mod)) cfields))

expDoc2 _ pr mPrec mod (CRecUpdate e cfields)
  = par mPrec $
      expDoc' e <+> record (commaSepList (map (fieldDoc expDoc') cfields))
  where
    expDoc' = expDoc unknown pr Nothing mod

fieldDoc :: (a -> Doc) -> CField a -> Doc
fieldDoc expOrPatDoc ((_, name), x) = text name <+> equals <+> expOrPatDoc x

statementDoc :: Precs -> String -> CStatement -> Doc
statementDoc pr mod (CSExpr e) = hang 1 $ expDoc False pr Nothing mod e
statementDoc pr mod (CSPat p e) =
    hang 1 $ patternDoc mod p <+> text "<-" <+> expDoc False pr Nothing mod e
statementDoc pr mod (CSLet ldecls)
  | null ldecls  = empty
  | otherwise    = text "let" <+>
                   align (vsep (punctuate line
		                          (map (localDeclDoc pr mod) ldecls)))

branchDoc :: Precs -> String -> (CPattern, CRhs) -> Doc
branchDoc pr mod (cpat, crhs)
  = def (patternDoc mod cpat) [] (align (rhsDoc pr mod arrow crhs))

-- -------------------------------pattern--------------------------------------

patternsDoc :: String -> [CPattern] -> Doc
patternsDoc mod = align .  fillSep .  map (patternDoc mod)

patternDoc :: String -> CPattern -> Doc
patternDoc _ (CPVar vname) = tvarDoc vname
patternDoc _ (CPLit l) = litDoc l
patternDoc mod cpc@(CPComb qn pns)
   | isJust mStringPatt = if null stringPatt
                                 then text "[]"
                                 else dquotes (text stringPatt)
   | isJust mListPatt = listDoc (map (patternDoc mod) listPatt)
   | null pns = qname mod qn
   | isTupleName qn = tupleDoc (map (patternDoc mod) pns)
   | isInfixName qn && length pns == 2 =
        parens $ patternDoc mod (pns!!0)  <+>
                 text (snd qn) <+>
                 patternDoc mod (pns!!1)
   | otherwise = parens (qname mod qn <+> group (hang 0 (vsep (map (patternDoc mod) pns))))
  where
      listDoc = fillEncloseSep lbracket rbracket (space<>comma)
      tupleDoc = fillEncloseSep lparen rparen (space<>comma)

      mListPatt = toListPattern cpc
      mStringPatt = toStringPattern cpc

      listPatt = fromJust mListPatt
      stringPatt = fromJust mStringPatt

patternDoc mod (CPAs vn p) = tvarDoc vn <> text "@" <> patternDoc mod p
patternDoc mod (CPFuncComb qn pns)
   | null pns = qname mod qn
   | isInfixName qn && length pns == 2
       = parens $ patternDoc mod (pns!!0) <> text (snd qn) <> patternDoc mod (pns!!1)
   | otherwise = parens (qname mod qn <+> hsep (map (patternDoc mod) pns))
patternDoc mod (CPLazy p) = text "~" <> patternDoc mod p
patternDoc mod (CPRecord (_, name) cfields)
  = text name <+> record (commaSepList (map (fieldDoc (patternDoc mod)) cfields))

toListPattern :: CPattern -> Maybe [CPattern]
toListPattern patt
  = case patt of
      CPComb ("Prelude","[]") [] -> Just []
      CPComb ("Prelude",":") [x,xs] -> toListPattern xs >>- Just . (x:)
      _ -> Nothing

toStringPattern :: CPattern -> Maybe String
toStringPattern patt
  = case patt of
      CPComb ("Prelude","[]") [] -> Just ""
      CPComb ("Prelude",":") [CPLit (CCharc c),cs] ->
        toStringPattern cs >>- Just . (quoteChar c++)
      _ -> Nothing
