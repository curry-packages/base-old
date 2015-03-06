-- ---------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Curry programs
--- in Curry (type "CurryProg") and an I/O action to read Curry programs and
--- transform them into this abstract representation (function "readCurry").
---
--- Note this defines a slightly new format for AbstractCurry
--- in comparison to the first proposal of 2003.
---
--- Assumption: an abstract Curry program is stored in file with
--- extension .acy
---
--- @author Michael Hanus, Björn Peemöller
--- @version November 2015
-- ---------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module AbstractCurry where

import Char         (isSpace)
import Directory    (doesFileExist)
import Maybe        (isNothing)
import ReadShowTerm
import Distribution
import FilePath     ((<.>))

-- ---------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ---------------------------------------------------------------------------

--- Current version of AbstractCurry
version :: String
version = "AbstractCurry 1.0"

--- A module name.
type MName = String

--- The data type for representing qualified names.
--- In AbstractCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
type QName = (MName, String)

--- Data type to specify the visibility of various entities.
data CVisibility
  = Public    -- exported entity
  | Private   -- private entity
  deriving (Eq, Show)

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (CProg modname imports typedecls functions opdecls)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls: Type declarations
---       functions: Function declarations
---       opdecls: Operator precedence declarations
data CurryProg = CurryProg MName [MName] [CTypeDecl] [CFuncDecl] [COpDecl]
  deriving (Eq, Show)

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
---
--- A data type definition of the form
---
--- <code>data t x1...xn = ...| c t1....tkc |...</code>
---
--- is represented by the Curry term
---
--- <code>(CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])</code>
---
--- where each <code>ij</code> is the index of the type variable
--- <code> xj</code>.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
data CTypeDecl
  = CType    QName CVisibility [CTVarIName] [CConsDecl]
  | CTypeSyn QName CVisibility [CTVarIName] CTypeExpr
  | CNewType QName CVisibility [CTVarIName] CConsDecl
  deriving (Eq, Show)

--- The type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type CTVarIName = (Int, String)

--- A constructor declaration consists of the name of the
--- constructor and a list of the argument types of the constructor.
--- The arity equals the number of types.
data CConsDecl
  = CCons   QName CVisibility [CTypeExpr]
  | CRecord QName CVisibility [CFieldDecl]
  deriving (Eq, Show)

--- A record field declaration consists of the name of the
--- the label, the visibility and its corresponding type.
data CFieldDecl = CField QName CVisibility CTypeExpr
  deriving (Eq, Show)

--- Type expression.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data CTypeExpr
  = CTVar CTVarIName               -- type variable
  | CFuncType CTypeExpr CTypeExpr  -- function type t1->t2
  | CTCons QName [CTypeExpr]       -- type constructor application
                                   -- (CTCons (module,name) arguments)
  deriving (Eq, Show)

--- Labeled record fields
type CField a = (QName, a)

--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).
data COpDecl = COp QName CFixity Int
  deriving (Eq, Show)

--- Data type for operator associativity
data CFixity
  = CInfixOp   -- non-associative infix operator
  | CInfixlOp  -- left-associative infix operator
  | CInfixrOp  -- right-associative infix operator
  deriving (Eq, Show)

--- Function arity
type Arity = Int

--- Data type for representing function declarations.
---
--- A function declaration in AbstractCurry is a term of the form
---
--- <code>(CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))</code>
---
--- and represents the function <code>name</code> defined by the rules
--- <code>rule1,...,rulek</code>.
---
--- Note: the variable indices are unique inside each rule
---
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules.
---
--- A function declaration with the constructor <code>CmtFunc</code>
--- is similarly to <code>CFunc</code> but has a comment
--- as an additional first argument. This comment could be used
--- by pretty printers that generate a readable Curry program
--- containing documentation comments.
data CFuncDecl
  = CFunc          QName Arity CVisibility CTypeExpr [CRule]
  | CmtFunc String QName Arity CVisibility CTypeExpr [CRule]
  deriving (Eq, Show)

--- The general form of a function rule. It consists of a list of patterns
--- (left-hand side), a list of guards (@success@ if not present in the
--- source text) with their corresponding right-hand sides, and
--- a list of local declarations.
data CRule = CRule [CPattern] CRhs
  deriving (Eq, Show)

--- Right-hand-side of a 'CRule' or an @case@ expression
data CRhs
  = CSimpleRhs  CExpr            [CLocalDecl] -- @expr where decls@
  | CGuardedRhs [(CExpr, CExpr)] [CLocalDecl] -- @| cond = expr where decls@
  deriving (Eq, Show)

--- Data type for representing local (let/where) declarations
data CLocalDecl
  = CLocalFunc CFuncDecl     -- local function declaration
  | CLocalPat  CPattern CRhs -- local pattern declaration
  | CLocalVars [CVarIName]   -- local free variable declaration
  deriving (Eq, Show)

--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.
type CVarIName = (Int,String)

--- Data type for representing pattern expressions.
data CPattern
  = CPVar      CVarIName               -- pattern variable (unique index / name)
  | CPLit      CLiteral                -- literal (Integer/Float/Char constant)
  | CPComb     QName [CPattern]        -- application (m.c e1 ... en) of n-ary
                                       -- constructor m.c (CPComb (m,c) [e1,...,en])
  | CPAs       CVarIName CPattern      -- as-pattern (extended Curry)
  | CPFuncComb QName [CPattern]        -- function pattern (extended Curry)
  | CPLazy     CPattern                -- lazy pattern (extended Curry)
  | CPRecord   QName [CField CPattern] -- record pattern (extended Curry)
  deriving (Eq, Show)

--- Data type for representing Curry expressions.
data CExpr
  = CVar       CVarIName                          -- variable (unique index / name)
  | CLit       CLiteral                           -- literal (Integer/Float/Char constant)
  | CSymbol    QName                              -- a defined symbol with module and name
  | CApply     CExpr CExpr                        -- application (e1 e2)
  | CLambda    [CPattern] CExpr                   -- lambda abstraction
  | CLetDecl   [CLocalDecl] CExpr                 -- local let declarations
  | CDoExpr    [CStatement]                       -- do expression
  | CListComp  CExpr [CStatement]                 -- list comprehension
  | CCase      CCaseType CExpr [(CPattern, CRhs)] -- case expression
  | CTyped     CExpr CTypeExpr                    -- typed expression
  | CRecConstr QName [CField CExpr]               -- record construction (extended Curry)
  | CRecUpdate CExpr [CField CExpr]               -- record update (extended Curry)
  deriving (Eq, Show)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.
data CLiteral
  = CIntc   Int
  | CFloatc Float
  | CCharc  Char
  | CStringc String
  deriving (Eq, Show)

--- Data type for representing statements in do expressions and
--- list comprehensions.
data CStatement
  = CSExpr CExpr         -- an expression (I/O action or boolean)
  | CSPat CPattern CExpr -- a pattern definition
  | CSLet [CLocalDecl]   -- a local let declaration
  deriving (Eq, Show)

--- Type of case expressions
data CCaseType
  = CRigid -- rigid case expression
  | CFlex  -- flexible case expression
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------

--- I/O action which parses a Curry program and returns the corresponding
--- typed Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry") and the result is a Curry term representing this
--- program.
readCurry :: String -> IO CurryProg
readCurry prog = readCurryWithParseOptions prog (setQuiet True defaultParams)

--- I/O action which parses a Curry program and returns the corresponding
--- untyped Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry") and the result is a Curry term representing this
--- program.
readUntypedCurry :: String -> IO CurryProg
readUntypedCurry prog =
  readUntypedCurryWithParseOptions prog (setQuiet True defaultParams)

--- I/O action which reads a typed Curry program from a file (with extension
--- ".acy") with respect to some parser options.
--- This I/O action is used by the standard action <CODE>readCurry</CODE>.
--- It is currently predefined only in Curry2Prolog.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end

readCurryWithParseOptions :: String -> FrontendParams -> IO CurryProg
readCurryWithParseOptions progname options = do
  mbmoddir <- lookupModuleSourceInLoadPath progname
                >>= return . maybe Nothing (Just . fst)
  unless (isNothing mbmoddir) $
    callFrontendWithParams ACY options progname
  filename <- findFileInLoadPath (abstractCurryFileName progname)
  readAbstractCurryFile filename

--- I/O action which reads an untyped Curry program from a file (with extension
--- ".uacy") with respect to some parser options. For more details
--- see function 'readCurryWithParseOptions'
readUntypedCurryWithParseOptions :: String -> FrontendParams -> IO CurryProg
readUntypedCurryWithParseOptions progname options = do
  mbmoddir <- lookupModuleSourceInLoadPath progname
                >>= return . maybe Nothing (Just . fst)
  unless (isNothing mbmoddir) $
    callFrontendWithParams UACY options progname
  filename <- findFileInLoadPath (untypedAbstractCurryFileName progname)
  readAbstractCurryFile filename

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding AbstractCurry program.
abstractCurryFileName :: String -> String
abstractCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "acy"

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding untyped AbstractCurry program.
untypedAbstractCurryFileName :: String -> String
untypedAbstractCurryFileName prog =
  inCurrySubdir (stripCurrySuffix prog) <.> "uacy"

--- I/O action which reads an AbstractCurry program from a file in ".acy"
--- format. In contrast to <CODE>readCurry</CODE>, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".acy") containing an AbstractCurry program in ".acy"
--- format and the result is a Curry term representing this program.
--- It is currently predefined only in Curry2Prolog.
readAbstractCurryFile :: String -> IO CurryProg
readAbstractCurryFile filename = do
  exacy <- doesFileExist filename
  if exacy
   then readExistingACY filename
   else do let subdirfilename = inCurrySubdir filename
           exdiracy <- doesFileExist subdirfilename
           if exdiracy
            then readExistingACY subdirfilename
            else error ("EXISTENCE ERROR: AbstractCurry file '"++filename++
                        "' does not exist")
 where
   readExistingACY fname = do
     filecontents <- readFile fname
     let (line1,lines) = break (=='\n') filecontents
     if line1 == "{- "++version++" -}"
      then return (readUnqualifiedTerm ["AbstractCurry","Prelude"] lines)
      else error $ "AbstractCurry: incompatible file found: "++fname

--- Tries to read an AbstractCurry file and returns
---
---  * Left err  , where err specifies the error occurred
---  * Right prog, where prog is the AbstractCurry program
tryReadACYFile :: String -> IO (Maybe CurryProg)
tryReadACYFile fn = do
  exists <- doesFileExist fn
  if exists
    then tryRead fn
    else do
      let fn' = inCurrySubdir fn
      exists' <- doesFileExist fn'
      if exists'
        then tryRead fn'
        else cancel
 where
  tryRead file = do
    src <- readFile file
    let (line1,lines) = break (=='\n') src
    if line1 /= "{- "++version++" -}"
      then error $ "AbstractCurry: incompatible file found: "++fn
      else case readsUnqualifiedTerm ["AbstractCurry","Prelude"] lines of
        []       -> cancel
        [(p,tl)] -> if all isSpace tl
                      then return $ Just p
                      else cancel
        _        -> cancel
  cancel = return Nothing

--- Writes an AbstractCurry program into a file in ".acy" format.
--- The first argument must be the name of the target file
--- (with suffix ".acy").
writeAbstractCurryFile :: String -> CurryProg -> IO ()
writeAbstractCurryFile file prog = writeFile file (showTerm prog)

------------------------------------------------------------------------------
