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
import Data.List (sortBy)

encode :: Polynomial Text Int Int -> TRS Text Text
encode poly
  | Map.null pos || Map.null neg =
    error
      "HilbertEncoding.Degrees.encode: \
      \ polynomial must contain both positive and negative monomials"
  | otherwise =
    (f(tPos) .->. tNeg) : ruleX (Poly.polyVars poly) : trsD
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
  | otherwise = go (Poly.powerprodToMap pp)
 where
  go mp = case Map.maxViewWithKey mp of
    Nothing -> iterate (\t -> m(x, t)) x !! (c - 1)
    Just ((v,e),mp') -> iterate (varFun v) (go mp') !! e

  -- go [] = iterate (\t -> m(x, t)) x !! (c - 1)
  -- go ((v, e) : xs) = iterate (varFun v) (go xs) !! e

encodePolynomial :: Polynomial Text Int Int -> Term Text Text
encodePolynomial poly =
  case sortBy (flip $ Poly.totalDegreeOrder vs) $  Poly.monomials poly of
    [] ->
      error
        "HilbertEncoding.Degrees.encodePolynomial: \
        \ polynomial must be non zero"
    ys -> foldr1 (curry m) (fmap encodeMonomial ys)
 where
  vs = Set.toList $ Poly.polyVars poly

ruleX :: Set.Set Text -> Rule Text Text
ruleX vars = f(o) .->. m(q(o), a (o, varTerm))
 where
  varTerm
    | Set.null vars = o
    | otherwise = foldr1 (curry a) [varFun v o | v <- Set.toList vars]

trsD :: TRS Text Text
trsD =
  [ q(f(x)) .->. f(f(q(x)))
  , a(q(x), f(f(x))) .->. q(a(x, f(o)))
  , f(x) .->. a(x, x)
  , f(x) .->. m(o, x)
  , f(x) .->. m(x, o)
  , f(q(x)) .->. m(x, x)
  , m(x, x) .->. q(x)
  ]

-- Syntactic Sugar ------------------------------------------------------------

-- Function/Variable Symbols --------------------------------------------------
varFun :: Text -> Term Text Text -> Term Text Text
varFun v t = Fun v [t]

-- Constants
o :: Term Text Text
o = Fun "0" []

-- Unary
f, q :: Term Text Text -> Term Text Text
f t = Fun "f" [t]
q t = Fun "q" [t]

-- Binary
a, m :: (Term Text Text, Term Text Text) -> Term Text Text
a (t1, t2) = Fun "a" [t1, t2]
m (t1, t2) = Fun "m" [t1, t2]

-- Binary
x :: Term Text Text
x = Var "x"
