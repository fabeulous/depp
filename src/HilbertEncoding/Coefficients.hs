{-# LANGUAGE OverloadedStrings #-}

-- |
module HilbertEncoding.Coefficients (encode) where

import HilbertEncoding.Utils
import Polynomial.Type (Monomial (..), Polynomial (Polynomial))
import qualified Polynomial.Type as Poly

import qualified Data.Map as Map
import Data.Rewriting.Term (Term (..))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List (sortBy)

encode :: Polynomial Text Int Int -> TRS Text Text
encode poly = polyRules ++ trsC ++ encodeVarConstraints vars
 where
  vars = Poly.polyVars poly
  polyRules = encodePolynomial poly

encodeVarConstraints :: Set.Set Text -> TRS Text Text
encodeVarConstraints vars =
  [ r
  | v <- Set.toList vars
  , r <- [s(o) .->. varFun v o, f(x) .->. varFun v x]
  ]

encodeMonomial :: Monomial Text Int Int -> Term Text Text
encodeMonomial (Monomial c pp)
  | c < 0 =
    error
      "HilbertEncoding.Coefficients.encodeMonomial: \
      \coefficient must be non negative"
  | otherwise = go (Poly.exponents pp)
 where
  go [] = iterate s o !! c
  go ((v, e) : xs)
    | e < 0 =
      error
        "HilbertEncoding.Coefficients.encodeMonomial: \
        \exponent must be non negative"
    | otherwise = iterate (varFun v) (go xs) !! e

encodePositivePolynomial :: Polynomial Text Int Int -> Term Text Text
encodePositivePolynomial poly =
  case sortBy (flip $ Poly.totalDegreeOrder vs) (Poly.monomials poly) of
    [] -> o
    xs -> foldr1 (curry a) (map encodeMonomial xs)
 where
  vs = Set.toList $ Poly.polyVars poly

encodePolynomial :: Polynomial Text Int Int -> TRS Text Text
encodePolynomial poly =
  [ _A(s(tPos)) .->. _B(tNeg)
  , _A(s(tNeg)) .->. _B(tPos)
  ]
 where
  mp = Poly.polyToMap poly
  (pos, neg) = Map.partition (>= 0) mp

  tPos = encodePositivePolynomial (Polynomial pos)
  tNeg = encodePositivePolynomial (Polynomial (fmap negate neg))

trsC :: TRS Text Text
trsC =
  [ f(s(x)) .->. s(s(f(x)))
  , q(f(x)) .->. f(f(q(x)))
  , f(x) .->. a(x, x)
  , s(x) .->. a(o, x)
  , s(x) .->. a(x, o)
  , a(q(x), f(x)) .->. q(s(x))
  , s(s(o)) .->. q(s(o))
  , s(_A(x)) .->. _B(x)
  , s(_B(x)) .->. _A(x)
  ]

-- Syntactic Sugar ------------------------------------------------------------

varFun :: Text -> Term Text Text -> Term Text Text
varFun v t = Fun v [t]

-- Function/Variable Symbols --------------------------------------------------

-- Constants
o :: Term Text Text
o = Fun "0" []

-- Unary
f, s, q, _A, _B :: Term Text Text -> Term Text Text
f t = Fun "f" [t]
s t = Fun "s" [t]
q t = Fun "q" [t]
_A t = Fun "A" [t]
_B t = Fun "B" [t]

-- Binary
a :: (Term Text Text, Term Text Text) -> Term Text Text
a (t1, t2) = Fun "a" [t1, t2]

-- Binary
x :: Term Text Text
x = Var "x"
