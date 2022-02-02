-- |
module HilbertEncoding.Utils where

import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Term (Term)

type TRS f v = [Rule f v]

-- | Rule constructor
(.->.) :: Term f v -> Term f v -> Rule f v
(.->.) = Rule
