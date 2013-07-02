------------------------------------------------------------------------------
--- An annotatable version of FlatCurry.
---
--- This module contains a version of FlatCurry's abstract syntax tree which
--- can be annotated with arbitrary information due to a polymorphic type
--- parameter.
--- For instance, this could be used to annotate function declarations
--- and expressions with their corresponding type.
---
--- For more information about the abstract syntax tree of `FlatCurry`,
--- see the documentation of the respective module.
---
--- @author  Jonas Oberschweiber, Björn Peemöller, Michael Hanus
--- @version June 2013
------------------------------------------------------------------------------

module AnnotatedFlatCurry
  ( module AnnotatedFlatCurry
  , module FlatCurry
  ) where

import FlatCurry ( QName, VarIndex, Visibility (..), TVarIndex
                 , TypeDecl (..), OpDecl (..), Fixity (..)
                 , TypeExpr (..), ConsDecl (..)
                 , Literal (..), CombType (..), CaseType (..)
                 )

--- Annotated FlatCurry program (corresponds to a module)
data AProg a = AProg String [String] [TypeDecl] [AFuncDecl a] [OpDecl]

--- Arity of a function declaration
type Arity = Int

--- Annotated function declaration
data AFuncDecl a = AFunc QName Arity Visibility TypeExpr (ARule a)

--- Annotated function rule
data ARule a
  = ARule [VarIndex] (AExpr a)
  | AExternal String

--- Annotated expression
data AExpr a
  = AVar   a VarIndex
  | ALit   a Literal
  | AComb  a CombType QName [AExpr a]
  | ALet   a [(VarIndex, AExpr a)] (AExpr a)
  | AFree  a [(VarIndex, a)] (AExpr a)
  | AOr    a (AExpr a) (AExpr a)
  | ACase  a CaseType (AExpr a) [ABranchExpr a]
  | ATyped a (AExpr a) TypeExpr

--- Annotated case branch
data ABranchExpr a = ABranch (APattern a) (AExpr a)

--- Annotated pattern
data APattern a
  = APattern a QName [VarIndex] --- constructor pattern
  | ALPattern a Literal         --- literal pattern

--- Extract the annotation of an annotated expression.
exprAnnotation :: AExpr a -> a
exprAnnotation (AComb  ann _ _ _) = ann
exprAnnotation (ACase  ann _ _ _) = ann
exprAnnotation (AVar   ann _    ) = ann
exprAnnotation (ALit   ann _    ) = ann
exprAnnotation (AOr    ann _ _  ) = ann
exprAnnotation (ALet   ann _ _  ) = ann
exprAnnotation (AFree  ann _ _  ) = ann
exprAnnotation (ATyped ann _ _  ) = ann

--- Extract the annotation of an annotated pattern.
patAnnotation :: APattern a -> a
patAnnotation (APattern  ann _ _) = ann
patAnnotation (ALPattern ann _  ) = ann

