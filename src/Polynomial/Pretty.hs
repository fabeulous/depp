{-# LANGUAGE OverloadedStrings #-}

-- |
module Polynomial.Pretty where

import qualified Polynomial.Type as Poly

import Data.List (sortOn, sortBy)
import qualified Data.Map as Map
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), hsep, sep, (<+>))

prettyPolynomial :: Poly.Polynomial Text Int Int -> Doc ann
prettyPolynomial poly = case sortBy (flip $ Poly.totalDegreeOrder vs) mons of
  [] -> "0"
  m : ms -> hsep $ prettyMonomial m : map withSign ms
 where
  mons = Poly.monomials poly
  vs = Set.toList $ Poly.polyVars poly

  withSign m@(Poly.Monomial c pp)
    | c < 0 = "-" <+> prettyMonomial (Poly.Monomial (- c) pp)
    | otherwise = "+" <+> prettyMonomial m

prettyMonomial :: Poly.Monomial Text Int Int -> Doc ann
prettyMonomial (Poly.Monomial c pp)
  | emptyPP pp = pretty c
  | c == 1 = prettyPowerProd pp
  | c == -1 = "-" <+> prettyPowerProd pp
  | otherwise = pretty c <+> prettyPowerProd pp
  where
    emptyPP = Map.null . Poly.powerprodToMap

prettyPowerProd :: Poly.PowerProduct Text Int -> Doc ann
prettyPowerProd pp = sep [prettyExp v e | (v, e) <- Poly.exponents pp]
 where
  prettyExp v e
    | e == 1 = pretty v
    | otherwise = pretty v <> "^" <> pretty e
