{-# LANGUAGE OverloadedStrings #-}

-- |
module Polynomial.Pretty where

import qualified Polynomial.Type as Poly

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), hsep, sep, (<+>))

prettyPolynomial :: Poly.Polynomial Text Int Int -> Doc ann
prettyPolynomial poly = case mons of
  [] -> "0"
  m : ms -> hsep $ prettyMonomial m : map withSign ms
 where
  mons = Poly.monomials poly

  withSign m@(Poly.Monomial c pp)
    | c < 0 = "-" <+> prettyMonomial (Poly.Monomial (- c) pp)
    | otherwise = "+" <+> prettyMonomial m

prettyMonomial :: Poly.Monomial Text Int Int -> Doc ann
prettyMonomial (Poly.Monomial c pp)
  | c == 1 = prettyPowerProd pp
  | otherwise = pretty c <+> prettyPowerProd pp

prettyPowerProd :: Poly.PowerProduct Text Int -> Doc ann
prettyPowerProd pp = sep [prettyExp v e | (v, e) <- Poly.exponents pp]
 where
  prettyExp v e
    | e == 1 = pretty v
    | otherwise = pretty v <> "^" <> pretty e
