{-# LANGUAGE OverloadedStrings #-}

-- |
module HilbertEncoding.Functions (
  -- * Encoding Hilbert's 10th
  --
  -- Given a polynomial this generates a TRS which is polynomialy terminating
  -- if and only if the following decision problem is fulfilled:
  --
  -- _Input:_ as Polynomial $P$
  --
  -- _Question:_ is P(x1,...,xn) > 0 for all natural numbers x1, ..., xn?
  encode,
  -- * Encoding as differences of terms
  --
  -- To encode Polynomials and monomials we generate two terms @l@ and @r@.
  -- The difference of the interpretations <l> - <r> models
  -- the corresponding polynomial/monomial.
  encodePolynomial,
  encodeMonomial,
) where

import HilbertEncoding.Utils
import Polynomial.Type (Monomial (..), Polynomial)
import qualified Polynomial.Type as Poly

import qualified Data.Map as Map
import Data.Rewriting.Term (Term (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.List (sortBy)

-- | Generates the full TRS encoding the DP
encode :: Polynomial Text Int Int -> TRS Text Text
encode poly = (l .->. r) : trsRn
 where
  n =
    maximum
      [ e
      | Monomial _ pp <- Poly.monomials poly
      , e <- Map.elems (Poly.powerprodToMap pp)
      ]
  trsRn = trsR0 ++ exponents n
  (l, r) = encodePolynomial poly

-- | Generate two terms @(l,r)@ where @<l>@ - @<r>@ encodes the given polynomial
encodePolynomial :: Polynomial Text Int Int -> (Term Text Text, Term Text Text)
encodePolynomial poly
  | null mons = (o, o)
  | otherwise = (foldr1 (curry a) ls, foldr1 (curry a) rs)
  where
    mons = sortBy (flip $ Poly.totalDegreeOrder vs) $ Poly.monomials poly

    vs = Set.toList (Poly.polyVars poly)

    (ls,rs) = unzip (map encodeMonomial mons)

-- | Generate two terms @(l,r)@ where @<l>@ - @<r>@ encodes the given monomial
encodeMonomial :: Monomial Text Int Int -> (Term Text Text, Term Text Text)
encodeMonomial (Monomial c pp)
  | c < 0 = let (l,r) = go ppMap in (r,l)
  | otherwise = go ppMap
 where
  ppMap = Poly.powerprodToMap pp
  go mp = case Map.maxViewWithKey mp of
    Nothing -> (iterate s o !! abs c, o)
    Just ((v, i), mp') ->
      (a(m(l', p(i)(var v)), r'), a(m(r', p(i)(var v)), l'))
     where
      (l', r') = go mp'
      var = Var

exponents :: Int -> TRS Text Text
exponents n
  | n == 1 = [s(p(1)(x)) .->. x, s(x) .->. p(1)(x)]
  | n > 1 =
    [ s(a(p(n)(x), a(x, p(n - 1)(x)))) .->. m(x, p(n - 1)(x))
    , s(m(x, p(n - 1)(x))) .->. a(p(n)(x), a(x, p(n - 1)(x)))
    ]
      ++ exponents (n -1)
  | otherwise =
      error "HilbertEncoding.Functions.exponents: encountered exponent <= 0"

trsR0 :: TRS Text Text
trsR0 =
  [ f(s(x)) .->. s(s(f(x)))
  , a(q(x), f(x)) .->. q(s(x))
  , q(f(x)) .->. f(f(q(x)))
  , s(o) .->. q(o)
  , f(x) .->. a(x, x)
  , q(s(s(o))) .->. s(s(s(o)))
  , q(s(o)) .->. o
  , s(x) .->. a(o, x)
  , s(x) .->. a(x, o)
  , s(s(s(s(s(o))))) .->. q(s(s(o)))
  , s(d(x)) .->. a(x, x)
  , s(s(o)) .->. q(s(o))
  , s(a(x, x)) .->. d(x)
  , s(a(q(a(x, y)), d(a(x, y)))) .->. a(a(q(x), q(y)), d(m(x, y)))
  , s(a(a(q(x), q(y)), d(m(x, y)))) .->. a(q(a(x, y)), d(a(x, y)))
  ]

-- Syntactic Sugar ------------------------------------------------------------

-- Function/Variable Symbols --------------------------------------------------

-- Constants
o :: Term Text Text
o = Fun "0" []

-- Unary
f, s, q, d :: Term Text Text -> Term Text Text
f t = Fun "f" [t]
s t = Fun "s" [t]
q t = Fun "q" [t]
d t = Fun "d" [t]

-- | Function symbols 'p_n' used for exponents
p :: Int -> Term Text Text -> Term Text Text
p n t = Fun ("p" <> Text.pack (show n)) [t]

-- Binary
a, m :: (Term Text Text, Term Text Text) -> Term Text Text
a (t1, t2) = Fun "a" [t1, t2]
m (t1, t2) = Fun "m" [t1, t2]

-- Binary
x, y :: Term Text Text
x = Var "x"
y = Var "y"
