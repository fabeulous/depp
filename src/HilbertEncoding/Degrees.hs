{-# LANGUAGE OverloadedStrings #-}

-- |
module HilbertEncoding.Degrees (encode) where

import HilbertEncoding.Utils
import Polynomial.Type (Monomial (..), Polynomial (Polynomial))
import qualified Polynomial.Type as Poly

import qualified Data.Map as Map
import Data.Rewriting.Rule (Rule)
import Data.Rewriting.Term (Term (..))
import qualified Data.Set as Set
import Data.Text (Text)

encode :: Polynomial Text Int Int -> TRS Text Text
encode poly
  | Map.null pos || Map.null neg =
    error
      "HilbertEncoding.Degrees.encode: \
      \ polynomial must conatin both positive and negative monomials"
  | otherwise =
    (g(tPos) .->. tNeg) : ruleX (Poly.polyVars poly) : trsD
 where
  mp = Poly.polyToMap poly
  (pos, neg) = Map.partition (>= 0) mp
  tPos = encodePolynomial (Polynomial pos)
  tNeg = encodePolynomial (Polynomial (fmap negate neg))

encodeMonomial :: Monomial Text Int Int -> Term Text Text
encodeMonomial (Monomial c pp)
  | c < 1 =
    error
      "HilbertEncoding.Degrees.encodeMonomial: \
      \ coefficients most be at least 1"
  | otherwise = go (Poly.exponents pp)
 where
  go [] = iterate (\t -> m(x, t)) x !! (c - 1)
  go ((v, e) : xs) = iterate (varFun v) (go xs) !! e

encodePolynomial :: Polynomial Text Int Int -> Term Text Text
encodePolynomial poly =
  case Poly.monomials poly of
    [] ->
      error
        "HilbertEncoding.Degrees.encodePolynomial: \
        \ polynomial must be non zero"
    ys -> foldr1 (curry m) (fmap encodeMonomial ys)

ruleX :: Set.Set Text -> Rule Text Text
ruleX vars = g(o) .->. m(q(o), a (o, varTerm))
 where
  varTerm
    | Set.null vars = o
    | otherwise = foldr1 (curry a) [varFun v o | v <- Set.toList vars]

trsD :: TRS Text Text
trsD =
  [ q(g(x)) .->. g(g(q(x)))
  , g(x) .->. a(x, x)
  , a(q(x), g(g(x))) .->. q(a(x, g(o)))
  , g(x) .->. m(o, x)
  , g(q(x)) .->. m(x, x)
  , g(x) .->. m(x, o)
  , m(x, x) .->. q(x)
  ]

-- Syntactic Sugar ------------------------------------------------------------

-- Function/Variable Symbols --------------------------------------------------
varFun :: Text -> Term Text Text -> Term Text Text
varFun v t = Fun ("_" <> v) [t]

-- Constants
o :: Term Text Text
o = Fun "0" []

-- Unary
g, q :: Term Text Text -> Term Text Text
g t = Fun "g" [t]
q t = Fun "q" [t]

-- Binary
a, m :: (Term Text Text, Term Text Text) -> Term Text Text
a (t1, t2) = Fun "a" [t1, t2]
m (t1, t2) = Fun "m" [t1, t2]

-- Binary
x :: Term Text Text
x = Var "x"
