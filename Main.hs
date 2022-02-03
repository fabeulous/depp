{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import qualified HilbertEncoding.Coefficients as Coefficients
import qualified HilbertEncoding.Degrees as Degrees
import qualified HilbertEncoding.Functions as Functions
import qualified Polynomial.Parse as Poly
import Rewriting.Pretty (prettyTRS)

import Data.Char (toLower)
import Data.Text (Text)
import Polynomial (Polynomial (Polynomial), prettyPolynomial)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import Foreign.C (charIsRepresentable)

data Encoding = Functions | Coefficients | Degrees

parseEnc :: String -> Encoding
parseEnc s = case map toLower s of
  "functions" -> Functions
  "coefficients" -> Coefficients
  "degrees" -> Degrees
  _ -> error "Not a valid Encoding"

data Format = PrettyFmt

parseFmt :: String -> Format
parseFmt = \case
  "pretty" -> PrettyFmt
  _ -> error "Not a valid Format"

data Settings = Settings
  { encoding :: Encoding
  , outputFormat :: Format
  }

defSettings =
  Settings
    { encoding = Functions
    , outputFormat = PrettyFmt
    }

optionDescription :: [OptDescr (Settings -> IO Settings)]
optionDescription =
  [ Option
      "e"
      ["enc"]
      (ReqArg (\s opt -> return $ opt {encoding = parseEnc s}) "ENC")
      "which encoding to use"
  , Option
      "o"
      ["fmt", "output-format"]
      (ReqArg (\s opt -> return $ opt {outputFormat = parseFmt s}) "FMT")
      "output format, default: pretty"
  ]

main :: IO ()
main = do
  args <- getArgs
  case Opt.getOpt Opt.Permute optionDescription args of
    (opts', polyString : _, []) -> do
      opts <- foldl (>>=) (pure defSettings) opts'
      case Poly.parsePolynomial polyString of
        Left e -> error (show e)
        Right poly -> main' opts poly
    _ -> error "Use one argument"

main' :: Settings -> Polynomial Text Int Int -> IO ()
main' opt poly = do
  putStrLn "Input polynomial:"
  putStrLn ""
  print $ prettyPolynomial poly
  putStrLn ""
  putStrLn "Output TRS:"
  print $ prettyTRS trs
 where
  trs = encode poly
  encode = case encoding opt of
    Functions -> Functions.encode
    Coefficients -> Coefficients.encode
    Degrees -> Degrees.encode
