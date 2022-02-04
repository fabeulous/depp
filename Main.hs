{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import qualified HilbertEncoding.Coefficients as Coefficients
import qualified HilbertEncoding.Degrees as Degrees
import qualified HilbertEncoding.Functions as Functions
import qualified Output
import Polynomial (Polynomial)
import qualified Polynomial.Parse as Poly

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Text (Text)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)

data Encoding = Functions | Coefficients | Degrees
data Format = PrettyFmt | COPSFmt

data Settings f = Settings
  { encoding :: f Encoding
  , outputFormat :: Format
  }

defSettings :: Settings Maybe
defSettings =
  Settings
    { encoding = Nothing
    , outputFormat = PrettyFmt
    }

optionDescription :: [OptDescr (Settings Maybe -> IO (Settings Maybe))]
optionDescription =
  [ Option
      "h?"
      ["help"]
      (NoArg (\_ -> usage stdout >> exitSuccess))
      "print this message"
  , Option
      "f"
      ["func"]
      (NoArg (\opt -> return $ opt {encoding = Just Functions}))
      "encode as constraint on interpretation functions"
  , Option
      "c"
      ["coef"]
      (NoArg (\opt -> return $ opt {encoding = Just Coefficients}))
      "encode as constraint on coefficients"
  , Option
      "d"
      ["deg"]
      (NoArg (\opt -> return $ opt {encoding = Just Degrees}))
      "encode as constraint on degrees"
  , Option
      ""
      ["pretty"]
      (NoArg (\opt -> return $ opt {outputFormat = PrettyFmt}))
      "standard output format"
  , Option
      ""
      ["cops"]
      (NoArg (\opt -> return $ opt {outputFormat = COPSFmt}))
      "output in COPS format, useful as input to other tools\n(default: pretty)"
  ]

usage :: Handle -> IO ()
usage handle = do
  progName <- getProgName
  hPutStrLn handle $ Opt.usageInfo (helpHeader progName) optionDescription
  hPutStrLn handle helpFooter

helpHeader :: String -> String
helpHeader name =
  unlines
    [ "Usage: " ++ name ++ " [-f | -c | -d] [OPTIONS] POLY"
    , ""
    , "DESCRIPTION:"
    , ""
    , "Encode variants of Hilbert's 10th problem as (incremental) polynomial"
    , "termination problems."
    , ""
    , "One of the options --func (-f), --coef (-c) or --deg (-d) must be"
    , "given. If multiple encoding or output format options are specified,"
    , "the respectively last encoding or output format is used."
    , ""
    , "OPTIONS:"
    ]

helpFooter :: String
helpFooter =
  unlines
    [ "INPUT FORMAT (POLY):"
    , ""
    , "  <poly> :=  <monomial> [\"+\" | \"-\"] <poly> | <monomial>"
    , "  <monomial> := <powerproduct> | integer <powerproduct> | integer"
    , "  <powerproduct> := <exp> <powerproduct> | <exp>"
    , "  <exp> := var \"^\" integer | var"
    , ""
    , "Here 'integer' is some integer and 'var' a sequence of letters."
    ]

main :: IO ()
main = do
  args <- getArgs
  case Opt.getOpt Opt.Permute optionDescription args of
    (opts', polyString : _, []) -> do
      opts <- foldl (>>=) (return defSettings) opts'
      case encoding opts of
        Nothing -> do
          hPutStrLn stderr "Error: encoding must be chosen (-f, -d, -c)"
          usage stderr >> exitFailure
        Just enc -> do
          let settings = opts {encoding = Identity enc}
          case Poly.parsePolynomial polyString of
            Left e -> do
              hPutStrLn stderr $ "Parse error: " ++ show e
              exitFailure
            Right poly -> main' settings poly
    (_, [], _) -> do
      hPutStrLn stderr "Error: requires at least one of (-f,-d,-c) and a POLY."
      usage stderr
      exitFailure
    (_, _, e : _) -> do
      hPutStr stderr $ "Error: " ++ e

main' :: Settings Identity -> Polynomial Text Int Int -> IO ()
main' opt poly =
  case outputFormat opt of
    PrettyFmt -> Output.prettyOutput poly trs
    COPSFmt -> Output.copsOutput trs
 where
  trs = encode poly
  encode = case runIdentity $ encoding opt of
    Functions -> Functions.encode
    Coefficients -> Coefficients.encode
    Degrees -> Degrees.encode
