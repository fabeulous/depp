{-# LANGUAGE OverloadedStrings #-}

-- |
module Rewriting.Pretty where

import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Term (Term (..))
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter as Pretty

import HilbertEncoding.Utils (TRS)

data Annotation
  = AnnTerm
  | AnnFunSymb
  | AnnVarSymb
  | AnnRule
  | AnnSystem

prettyTRS :: (Pretty f, Pretty v) => TRS f v -> Doc Annotation
prettyTRS = Pretty.annotate AnnSystem . Pretty.vsep . map prettyRule

prettyRule :: (Pretty f, Pretty v) => Rule f v -> Doc Annotation
prettyRule = Pretty.annotate AnnRule . go
 where
  go (Rule l r) =
    Pretty.hang 2 $
      Pretty.group $
        Pretty.vsep [prettyTerm l, "->" <+> prettyTerm r]

prettyTerm :: (Pretty f, Pretty v) => Term f v -> Doc Annotation
prettyTerm = Pretty.annotate AnnTerm . Pretty.hang 2 . go
 where
  go (Var v) = Pretty.annotate AnnVarSymb $ pretty v
  go (Fun f []) = Pretty.annotate AnnFunSymb (pretty f)
  go (Fun f ts) =
    Pretty.annotate AnnFunSymb (pretty f)
      <> Pretty.align (Pretty.encloseSep "(" ")" "," (map go ts))
