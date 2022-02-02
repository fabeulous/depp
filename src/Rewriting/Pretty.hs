{-# LANGUAGE OverloadedStrings #-}

-- |
module Rewriting.Pretty where

import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Term (Term (..))
import Prettyprinter (Doc, Pretty (pretty), annotate, encloseSep, space, vsep, (<+>))

import HilbertEncoding.Utils (TRS)

data Annotation
  = AnnTerm
  | AnnFunSymb
  | AnnVarSymb
  | AnnRule
  | AnnSystem

prettyTRS :: (Pretty f, Pretty v) => TRS f v -> Doc Annotation
prettyTRS = annotate AnnSystem . vsep . map prettyRule

prettyRule :: (Pretty f, Pretty v) => Rule f v -> Doc Annotation
prettyRule = annotate AnnRule . go
 where
  go (Rule l r) = prettyTerm l <+> "->" <+> prettyTerm r

prettyTerm :: (Pretty f, Pretty v) => Term f v -> Doc Annotation
prettyTerm = annotate AnnTerm . go
 where
  go (Var v) = annotate AnnVarSymb $ pretty v
  go (Fun f []) = annotate AnnFunSymb (pretty f)
  go (Fun f ts) =
    annotate AnnFunSymb (pretty f)
      <> encloseSep "(" ")" ("," <> space) (map go ts)
