-- |

module HilbertEncoding.Coefficients (encode) where

import Polynomial.Type (Monomial (..), Polynomial)
import qualified Polynomial.Type as Poly
import HilbertEncoding.Utils

import Data.Text (Text)
import qualified Data.Text as Text

encode :: Polynomial Text Int Int -> TRS Text Text
encode = error "Not yet defined"
