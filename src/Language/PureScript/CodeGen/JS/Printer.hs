-- | Pretty printer for the JavaScript AST
module Language.PureScript.CodeGen.JS.Printer
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
  ) where

import Prelude.Compat

-- import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common (SMap)
import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

nest' = nest 2

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js = ("asd", [])
  -- let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  -- in (s, mp)

prettyPrintUnary :: UnaryOperator -> Doc ()
prettyPrintUnary Not = "!"
prettyPrintUnary New = "new "

prettyPrintAst :: AST -> Doc ()
prettyPrintAst (VariableIntroduction sm name Nothing) = pretty name
prettyPrintAst (VariableIntroduction sm name (Just val)) = "const" <+> pretty name <+> "=" <+> prettyPrintAst val
prettyPrintAst (NumericLiteral sm (Right val)) = pretty val
prettyPrintAst (NumericLiteral sm (Left val)) = pretty val
prettyPrintAst (Function sm name args block) = vsep
  [ nest' $ "function" <+> (maybe mempty ((<> space) . pretty) name) <> parens (sep $ pretty <$> args) <+> "{" <> line <>
    (prettyPrintAst block)
  , "}"
  ]
prettyPrintAst (App sm val args) = prettyPrintAst val <> nest' (line <> parens (concatWith (\a b -> a <> "," <> line <> b) $ prettyPrintAst <$> args))
prettyPrintAst (Indexer sm a b) = prettyPrintAst b <> brackets (prettyPrintAst a)
prettyPrintAst (StringLiteral sm str) = pretty $ prettyPrintStringJS str
prettyPrintAst (ObjectLiteral sm elements) = vsep
  [ nest' $ "{" <> line <> 
      group (concatWith (\a b -> a <> "," <> line <> b) (map go elements))
  , "}"
  ]
  where
    go (str, val) = (pretty $ prettyPrintStringJS str) <> ":" <+> prettyPrintAst val
prettyPrintAst (Assignment sm var val) = prettyPrintAst var <+> "=" <+> prettyPrintAst val
prettyPrintAst (Unary sm op expr) = prettyPrintUnary op <> prettyPrintAst expr
-- prettyPrintAst (Binary sm op a b) = prettyPrintUnary <> prettyprintAst expr
prettyPrintAst (Var sm name) = pretty name
prettyPrintAst (Block sm sts) = prettyPrintSts sts
prettyPrintAst (Return sm expr) = nest' $ "return" <> softline <> prettyPrintAst expr
prettyPrintAst s = pretty $ show s

prettyPrintSts :: [AST] -> Doc ()
prettyPrintSts sts = cat $ zipWith (<>) (prettyPrintAst <$> sts) (repeat ";")

prettyPrintJS :: [AST] -> Text
prettyPrintJS ast = renderStrict $ layoutPretty defaultLayoutOptions $ prettyPrintSts ast -- maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements
