{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Polynomial.Type where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Power Product
newtype PowerProduct v e = PowerProd {powerprodToMap :: Map.Map v e}
  deriving newtype (Eq, Ord, Show, Functor)

emptyPP :: PowerProduct v e
emptyPP = PowerProd Map.empty

multPP :: (Ord v, Num e) => PowerProduct v e -> PowerProduct v e -> PowerProduct v e
multPP p1 p2 = PowerProd $ Map.unionWith (+) (powerprodToMap p1) (powerprodToMap p2)

powerProd :: (Ord v, Num e) => [(v, e)] -> PowerProduct v e
powerProd = PowerProd . Map.fromListWith (+)

exponents :: PowerProduct v e -> [(v, e)]
exponents = Map.toList . powerprodToMap

instance (Semigroup e, Ord v) => Semigroup (PowerProduct v e) where
  pp1 <> pp2 =
    PowerProd (Map.unionWith (<>) (powerprodToMap pp1) (powerprodToMap pp2))

instance (Monoid e, Ord v) => Monoid (PowerProduct v e) where
  mempty = PowerProd Map.empty

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
  (Ord v, Ord e, Num c) => Polynomial v e c -> Polynomial v e c -> Polynomial v e c
addPoly p1 p2 = Polynomial $ Map.unionWith (+) (polyToMap p1) (polyToMap p2)

subtractPoly ::
  (Ord v, Ord e, Num c) => Polynomial v e c -> Polynomial v e c -> Polynomial v e c
subtractPoly p1 p2 = Polynomial $ Map.unionWith (+) (polyToMap p1) (negate <$> polyToMap p2)

instance (Semigroup c, Ord v, Ord e) => Semigroup (Polynomial v e c) where
  p1 <> p2 = Polynomial (Map.unionWith (<>) (polyToMap p1) (polyToMap p2))

instance (Monoid c, Ord v, Ord e) => Monoid (Polynomial v e c) where
  mempty = Polynomial Map.empty

polyOfMonomial :: (Ord e, Ord v) => Monomial v e c -> Polynomial v e c
polyOfMonomial (Monomial c pp) = Polynomial (Map.singleton pp c)

polynomial :: (Monoid c, Ord e, Ord v) => [Monomial v e c] -> Polynomial v e c
polynomial = foldMap polyOfMonomial

monomials :: Polynomial v e c -> [Monomial v e c]
monomials pp = [Monomial c vs | (vs, c) <- Map.toList coeffMap]
 where
  coeffMap = polyToMap pp

polyVars :: Ord v => Polynomial v e c -> Set.Set v
polyVars = Set.unions . map ppVars . Map.keys .  polyToMap

ppVars :: Ord v => PowerProduct v e -> Set.Set v
ppVars = Set.fromList . Map.keys . powerprodToMap
