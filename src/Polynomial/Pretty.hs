{-# LANGUAGE OverloadedStrings #-}

-- |
module Polynomial.Pretty where

import qualified Polynomial.Type as Poly

import qualified Data.List as List
import Prettyprinter (Doc, Pretty (pretty), encloseSep, hsep, sep, space, (<+>))

instance (Pretty c, Pretty e, Pretty v, Num c, Ord c, Eq c, Num e, Eq e) => Pretty (Poly.Polynomial v e c) where
  pretty = prettyPolynomial

prettyPolynomial :: (Pretty e, Pretty v, Pretty c, Num c, Eq c, Eq e, Num e, Ord c) => Poly.Polynomial v e c -> Doc ann
prettyPolynomial poly = case mons of
  [] -> "0"
  m : ms -> hsep $ prettyMonomial m : map withSign ms
 where
  mons = Poly.monomials poly

  withSign m@(Poly.Monomial c pp)
    | c < 0 = "-" <+> prettyMonomial (Poly.Monomial (- c) pp)
    | otherwise = "+" <+> prettyMonomial m

prettyMonomial :: (Pretty e, Pretty v, Pretty c, Num c, Eq c, Eq e, Num e) => Poly.Monomial v e c -> Doc ann
prettyMonomial (Poly.Monomial c pp)
  | c == 1 = prettyPowerProd pp
  | otherwise = pretty c <+> prettyPowerProd pp

prettyPowerProd :: (Pretty e, Pretty v, Eq e, Num e) => Poly.PowerProduct v e -> Doc ann
prettyPowerProd pp = sep [prettyExp v e | (v, e) <- Poly.exponents pp]
 where
  prettyExp v e
    | e == 1 = pretty v
    | otherwise = pretty v <> "^" <> pretty e
