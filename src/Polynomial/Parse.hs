{-# LANGUAGE FlexibleContexts #-}

-- |
module Polynomial.Parse (parsePolynomial) where

import Polynomial.Type (Monomial, Polynomial, PowerProduct)
import qualified Polynomial.Type as Poly

import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (ParsecT, Stream, (<|>))
import qualified Text.Parsec as P

parsePolynomial :: String -> Either P.ParseError (Polynomial Text Int Int)
parsePolynomial s = coerce $ P.parse (polynomial <* P.eof) "Polynomial" s

polynomial :: Stream s m Char => ParsecT s u m (Polynomial Text Int Int)
polynomial = P.chainl1 polySingleton op
 where
  polySingleton = Poly.polyOfMonomial <$> monomial
  op =
    tok "+" $> Poly.addPoly
      <|> tok "-" $> Poly.subtractPoly

monomial :: Stream s m Char => ParsecT s u m (Monomial Text Int Int)
monomial = do
  sgn <- P.option 1 sign
  c <- P.option 1 coeff
  pp <- powerProduct
  return $ Poly.monomial (sgn * c) pp
 where
  sign =
    tok "+" $> 1
      <|> tok "-" $> -1

powerProduct :: Stream s m Char => ParsecT s u m (PowerProduct Text Int)
powerProduct = Poly.powerProd <$> P.many powerVar

powerVar :: Stream s m Char => ParsecT s u m (Text, Int)
powerVar = (,) <$> var <*> P.option 1 (tok "^" *> expP)

var :: Stream s m Char => ParsecT s u m Text
var = T.pack <$> lexP var'
 where
  var' = (:) <$> P.letter <*> P.many P.digit

coeff, expP :: Stream s m Char => ParsecT s u m Int
expP = lexP nat
coeff = lexP nat

nat :: Stream s m Char => ParsecT s u m Int
nat = read <$> P.many1 P.digit

tok :: Stream s m Char => String -> ParsecT s u m String
tok s = lexP (P.string s)

lexP :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexP p = p <* P.spaces
