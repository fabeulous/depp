{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Polynomial.Type where

import Data.List (foldl1')
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding (map)

-- | Power Product
newtype PowerProduct v e = PowerProd {powerprodToMap :: Map.Map v e}
  deriving newtype (Eq, Ord, Show, Functor)

emptyPP :: PowerProduct v e
emptyPP = PowerProd Map.empty

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

addPoly ::
  (Ord v, Ord e, Num c, Eq c) => Polynomial v e c -> Polynomial v e c -> Polynomial v e c
addPoly p1 p2 = Polynomial $ addMaps (polyToMap p1) (polyToMap p2)

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
polynomial = foldl1' addPoly . fmap polyOfMonomial

monomials :: Polynomial v e c -> [Monomial v e c]
monomials pp = [Monomial c vs | (vs, c) <- Map.toList coeffMap]
 where
  coeffMap = polyToMap pp

polyVars :: Ord v => Polynomial v e c -> Set.Set v
polyVars = Set.unions . fmap ppVars . Map.keys . polyToMap

ppVars :: Ord v => PowerProduct v e -> Set.Set v
ppVars = Set.fromList . Map.keys . powerprodToMap

map :: (Ord v', Ord e') => (v -> v') -> (e -> e') -> (c -> c') -> Polynomial v e c -> Polynomial v' e' c'
map fv fe fc (Polynomial mp) = Polynomial (Map.mapKeys mapPP (Map.map fc mp))
 where
  mapPP = PowerProd . Map.mapKeys fv . Map.map fe . powerprodToMap
