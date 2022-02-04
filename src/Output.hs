{-# LANGUAGE OverloadedStrings #-}

-- |
module Output where

import HilbertEncoding.Utils (TRS)
import Polynomial (Polynomial, prettyPolynomial)
import Rewriting.Pretty (prettyTRS)

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Rewriting.Rules as R
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), Pretty (pretty))
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text as Pretty

prettyOutput :: Polynomial Text Int Int -> TRS Text Text -> IO ()
prettyOutput poly trs =
  print $
    Pretty.vsep
      [ "Input Polynomial:"
      , Pretty.indent 4 $ prettyPolynomial poly
      , "Encoding:"
      , Pretty.indent 4 $ prettyTRS trs
      ]

copsOutput :: TRS Text Text -> IO ()
copsOutput trs =
  Text.putStrLn . Pretty.renderStrict . Pretty.layoutSmart layoutopt $
    Pretty.vsep
      [ Pretty.group $ block "VAR" (Pretty.hsep (map Pretty.pretty vars))
      , block "RULES" (prettyTRS trs)
      ]
 where
  vars = nubOrd $ R.vars trs

  layoutopt = LayoutOptions Unbounded

block :: String -> Doc ann -> Doc ann
block name doc =
  Pretty.hang 2 ("(" <> pretty name <> Pretty.line <> doc) <> Pretty.line' <> ")"
