------------------------------------------------------------------------
--- This module provides some useful functions to write programs
--- that generate AbstractCurry programs more compact and readable.
------------------------------------------------------------------------

module AbstractCurryGoodies where

import AbstractCurry

infixr 9 ~>

------------------------------------------------------------------------
-- Goodies to construct type expressions

--- A function type.
(~>) :: CTypeExpr -> CTypeExpr -> CTypeExpr
t1 ~> t2 = CFuncType t1 t2

--- A base type.
baseType :: QName -> CTypeExpr
baseType t = CTCons t []

--- Constructs a list type from an element type.
listType :: CTypeExpr -> CTypeExpr
listType a = CTCons (pre "[]") [a]

--- Constructs a tuple type from list of component types.
tupleType :: [CTypeExpr] -> CTypeExpr
tupleType ts | l==0 = baseType (pre "()")
             | l==1 = head ts
             | otherwise = CTCons (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  ts
 where l = length ts

--- Constructs an IO type from a type.
ioType :: CTypeExpr -> CTypeExpr
ioType a = CTCons (pre "IO") [a]

--- Constructs a Maybe type from element type.
maybeType :: CTypeExpr -> CTypeExpr
maybeType a = CTCons (pre "Maybe") [a]

--- The type expression of the String type.
stringType :: CTypeExpr
stringType = baseType (pre "String")

--- The type expression of the Int type.
intType :: CTypeExpr
intType = baseType (pre "Int")

--- The type expression of the Bool type.
boolType :: CTypeExpr
boolType = baseType (pre "Bool")

--- The type expression of the Time.CalendarTime type.
dateType :: CTypeExpr
dateType = baseType ("Time", "CalendarTime")

------------------------------------------------------------------------
-- Goodies to construct function declarations

--- Constructs a function declaration from a given qualified function name,
--- arity, visibility, type expression and list of defining rules.
cfunc :: QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> CFuncDecl
cfunc name arity v t rules = 
  CFunc name arity v t (CRules CFlex rules)

--- Constructs a function declaration from a given comment,
--- qualified function name,
--- arity, visibility, type expression and list of defining rules.
cmtfunc :: String -> QName -> Int -> CVisibility -> CTypeExpr -> [CRule]
        -> CFuncDecl
cmtfunc comment name arity v t rules = 
  CmtFunc comment name arity v t (CRules CFlex rules)

--- Constructs a guarded expression with the trivial guard.
noGuard :: CExpr -> (CExpr, CExpr)
noGuard e = (CSymbol (pre "success"), e)

------------------------------------------------------------------------
-- Goodies to construct function expressions and patterns

--- An application of a qualified function name to a list of arguments.
applyF :: QName -> [CExpr] -> CExpr
applyF f es = foldl CApply (CSymbol f) es 

--- A constant, i.e., an application without arguments.
constF :: QName -> CExpr
constF f = applyF f []

--- An application of a variable to a list of arguments.
applyV :: CVarIName -> [CExpr] -> CExpr
applyV v es = foldl CApply (CVar v) es 

-- Applies the Just constructor to an AbstractCurry expression.
applyJust :: CExpr -> CExpr
applyJust a = applyF (pre "Just") [a]

-- Applies the maybe function to three AbstractCurry expressions.
applyMaybe :: CExpr -> CExpr -> CExpr -> CExpr
applyMaybe a1 a2 a3 = applyF (pre "maybe") [a1,a2,a3]

--- Constructs a tuple expression from list of component expressions.
tupleExpr :: [CExpr] -> CExpr
tupleExpr es | l==0 = constF (pre "()")
             | l==1 = head es
             | otherwise = applyF (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  es
 where l = length es

--- Constructs a tuple pattern from list of component patterns.
tuplePattern :: [CPattern] -> CPattern
tuplePattern ps
  | l==0 = CPComb (pre "()") []
  | l==1 = head ps
  | otherwise = CPComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

--- Constructs a list pattern from list of component patterns.
listPattern :: [CPattern] -> CPattern
listPattern []     = CPComb (pre "[]") []
listPattern (p:ps) = CPComb (pre ":") [p, listPattern ps]

--- Constructs a string into a pattern representing this string.
stringPattern :: String -> CPattern
stringPattern = listPattern . map (CPLit . CCharc)

--- Converts a list of AbstractCurry expressions into an
--- AbstractCurry representation of this list.
list2ac :: [CExpr] -> CExpr
list2ac []     = constF (pre "[]")
list2ac (c:cs) = applyF (pre ":") [c, list2ac cs]

--- Converts a string into an AbstractCurry represention of this string.  
string2ac :: String -> CExpr
string2ac = list2ac . map (CLit . CCharc)

--- Converts a string into a qualified name of the Prelude.
pre :: String -> QName
pre f = ("Prelude", f)

cvar :: String -> CExpr
cvar s = CVar (1,s)

ctvar :: String -> CTypeExpr
ctvar s = CTVar (1,s)

------------------------------------------------------------------------
