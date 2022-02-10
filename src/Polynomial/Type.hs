{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Polynomial.Type where

import Data.Function (on)
import Data.List (foldl')
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding (map)

-- | Power Product
newtype PowerProduct v e = PowerProd {powerprodToMap :: Map.Map v e}
  deriving newtype (Eq, Ord, Show, Functor)

emptyPP :: PowerProduct v e
emptyPP = PowerProd Map.empty

-- $setup
-- >>> :set -XFlexibleInstances -XStandaloneDeriving
-- >>> import qualified Test.QuickCheck as QC
-- >>> import qualified Test.QuickCheck.Modifiers as QC
-- >>> import qualified Test.QuickCheck.Poly as QC
-- >>> deriving instance (QC.Arbitrary v, Ord v, QC.Arbitrary e) => QC.Arbitrary (PowerProduct v e)
-- >>> deriving instance (QC.Arbitrary v, Ord v, QC.Arbitrary e, Ord e, QC.Arbitrary c) => QC.Arbitrary (Polynomial v e c)
-- >>> let getN = fmap QC.getNonZero
-- >>> type TestPolyType = Polynomial Char Int (QC.NonZero Int)

-- prop> \(p1 :: PowerProduct Char (QC.NonZero Int)) p2 -> all (/= 0) . Map.elems . powerprodToMap $ (getN p1 `multPP` getN p2)
-- +++ OK, passed 100 tests.
multPP :: (Ord v, Num e, Eq e) => PowerProduct v e -> PowerProduct v e -> PowerProduct v e
multPP p1 p2 = PowerProd $ addMaps (powerprodToMap p1) (powerprodToMap p2)

powerProd :: (Ord v, Num e, Eq e) => [(v, e)] -> PowerProduct v e
powerProd = PowerProd . Map.filter (/= 0) . Map.fromListWith (+)

exponents :: PowerProduct v e -> [(v, e)]
exponents = Map.toList . powerprodToMap

singleVar :: v -> e -> PowerProduct v e
singleVar v e = PowerProd (Map.singleton v e)

-- | Monomial
data Monomial v e c = Monomial c (PowerProduct v e)
  deriving (Eq, Ord, Show, Functor)

monomial :: c -> PowerProduct v e -> Monomial v e c
monomial = Monomial

isConstant :: Monomial v e c -> Bool
isConstant (Monomial _ pp) = Map.null (powerprodToMap pp)

-- | Polynomial
newtype Polynomial v e c = Polynomial {polyToMap :: Map.Map (PowerProduct v e) c}
  deriving newtype (Eq, Ord, Functor)
  deriving (Show)

-- prop> \(p1 :: TestPolyType) p2 -> all (/= 0) [c | Monomial c _ <- monomials (getN p1 `addPoly` getN p2)]
-- +++ OK, passed 100 tests.
addPoly ::
  (Ord v, Ord e, Num c, Eq c) => Polynomial v e c -> Polynomial v e c -> Polynomial v e c
addPoly p1 p2 = Polynomial $ addMaps (polyToMap p1) (polyToMap p2)

-- prop> \(p1 :: TestPolyType) p2 -> all (/= 0) [c | Monomial c _ <- monomials (getN p1 `subtract` getN p2)]
-- +++ OK, passed 100 tests.
subtractPoly ::
  (Ord v, Ord e, Num c, Eq c) => Polynomial v e c -> Polynomial v e c -> Polynomial v e c
subtractPoly p1 p2 = Polynomial $ addMaps (polyToMap p1) (negate <$> polyToMap p2)

addMaps :: (Ord k, Num c, Eq c) => Map.Map k c -> Map.Map k c -> Map.Map k c
addMaps = merge preserveMissing preserveMissing (zipWithMaybeMatched add')
 where
  add' _ a b = let res = a + b in if res == 0 then Nothing else Just res

polyOfMonomial :: (Ord e, Ord v) => Monomial v e c -> Polynomial v e c
polyOfMonomial (Monomial c pp) = Polynomial (Map.singleton pp c)

polynomial :: (Num c, Eq c, Ord e, Ord v) => [Monomial v e c] -> Polynomial v e c
polynomial = foldl' addPoly (Polynomial Map.empty) . fmap polyOfMonomial

-- prop> \(p' :: TestPolyType) -> let p = getN p' in p == polynomial (monomials p)
-- +++ OK, passed 100 tests.
monomials :: Polynomial v e c -> [Monomial v e c]
monomials pp = [Monomial c vs | (vs, c) <- Map.toList coeffMap]
 where
  coeffMap = polyToMap pp

polyVars :: Ord v => Polynomial v e c -> Set.Set v
polyVars = Set.unions . fmap ppVars . Map.keys . polyToMap

ppVars :: Ord v => PowerProduct v e -> Set.Set v
ppVars = Set.fromList . Map.keys . powerprodToMap

-- prop> \p' (QC.Fn fe) (QC.Fn fc) -> let p = getN p' in and [(c /= 0 && e /= 0) | Monomial c pp <- monomials (map id fe fc p), (_,e) <- exponents pp]
-- +++ OK, passed 100 tests.
map ::
  (Ord v', Ord e', Num e', Eq c', Num c') =>
  -- | Function to map over variables
  (v -> v') ->
  -- | Function to map over exponents
  (e -> e') ->
  -- | Function to map over coefficients
  (c -> c') ->
  Polynomial v e c ->
  Polynomial v' e' c'
map fv fe fc (Polynomial mp) = Polynomial (Map.mapKeys mapPP (Map.mapMaybe fc' mp))
 where
  mapPP = PowerProd . Map.mapKeys fv . Map.mapMaybe fe' . powerprodToMap

  fe' n = case fe n of
    0 -> Nothing
    n' -> Just n'
  fc' n = case fc n of
    0 -> Nothing
    n' -> Just n'

totalDegreeOrder ::
  (Num e, Ord v, Ord e, Ord c) => [v] -> Monomial v e c -> Monomial v e c -> Ordering
totalDegreeOrder vs = compare `on` lxgr
 where
  lxgr (Monomial c pp) = (sum (fmap snd es), fmap (ppMap !) vs, c)
   where
    es = exponents pp
    ppMap = powerprodToMap pp

    mp ! k = Map.findWithDefault 0 k mp
