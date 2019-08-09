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

nest' :: Doc () -> Doc ()
nest' = nest 2

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js = ("asd", [])
  -- let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  -- in (s, mp)

prettyPrintUnary :: UnaryOperator -> Doc ()
prettyPrintUnary Negate = "-"
prettyPrintUnary Not = "!"
prettyPrintUnary BitwiseNot = "~"
prettyPrintUnary Positive = "+"
prettyPrintUnary New = "new "

prettyPrintBinary :: BinaryOperator -> Doc ()
prettyPrintBinary Multiply             = "*"
prettyPrintBinary Divide               = "/"
prettyPrintBinary Modulus              = "%"
prettyPrintBinary Add                  = "+"
prettyPrintBinary Subtract             = "-"
prettyPrintBinary ShiftLeft            = "<<"
prettyPrintBinary ShiftRight           = ">>"
prettyPrintBinary ZeroFillShiftRight   = ">>>"
prettyPrintBinary LessThan             = "<"
prettyPrintBinary LessThanOrEqualTo    = "<="
prettyPrintBinary GreaterThan          = ">"
prettyPrintBinary GreaterThanOrEqualTo = ">="
prettyPrintBinary EqualTo              = "==="
prettyPrintBinary NotEqualTo           = "!=="
prettyPrintBinary BitwiseAnd           = "&"
prettyPrintBinary BitwiseXor           = "^"
prettyPrintBinary BitwiseOr            = "|"
prettyPrintBinary And                  = "&&"
prettyPrintBinary Or                   = "||"

sepComma :: [Doc ()] -> Doc ()
sepComma = concatWith (\a b -> a <> "," <> line <> b)

prettyPrintAst :: AST -> Doc ()
prettyPrintAst (VariableIntroduction _ name Nothing) = pretty name
prettyPrintAst (VariableIntroduction _ name (Just val)) = "const" <+> pretty name <+> "=" <+> prettyPrintAst val
prettyPrintAst (NumericLiteral _ (Right val)) = pretty val
prettyPrintAst (NumericLiteral _ (Left val)) = pretty val
prettyPrintAst (Function _ name args block) = vsep
  [ nest' $ "function" <+> (maybe mempty ((<> space) . pretty) name) <> parens (group $ sepComma $ pretty <$> args) <+> "{" <> line <>
    (prettyPrintAst block)
  , "}"
  ]
prettyPrintAst (App _ app args) = printFunc app <> (nest' (line' <> parens (group $ sepComma $ prettyPrintAst <$> args)))
    where
      printFunc val@(Function _ _ _ _) = parens (prettyPrintAst val)
      printFunc val = prettyPrintAst val
prettyPrintAst (Indexer _ a b) = prettyPrintAst b <> brackets (prettyPrintAst a)
prettyPrintAst (StringLiteral _ str) = pretty $ prettyPrintStringJS str
prettyPrintAst (ObjectLiteral _ elements) = vsep
  [ nest' $ "{" <> line <> 
      group (sepComma (map go elements))
  , "}"
  ]
  where
    go (str, val) = (pretty $ prettyPrintStringJS str) <> ":" <+> prettyPrintAst val
prettyPrintAst (Assignment _ var val) = prettyPrintAst var <+> "=" <+> prettyPrintAst val
prettyPrintAst (Unary _ op expr) = prettyPrintUnary op <> prettyPrintAst expr
prettyPrintAst (Binary _ op a b) = prettyPrintAst a <> prettyPrintBinary op <> prettyPrintAst b
prettyPrintAst (Var _ name) = pretty name
prettyPrintAst (Block _ sts) = prettyPrintSts sts
prettyPrintAst (Return _ expr) = nest' $ "return" <> softline <> prettyPrintAst expr
prettyPrintAst (BooleanLiteral _ True) = "true"
prettyPrintAst (BooleanLiteral _ False) = "false"
prettyPrintAst (ArrayLiteral _ expr) = list $ map prettyPrintAst expr
prettyPrintAst (While _ cond expr) = "while" <+> parens (prettyPrintAst cond) <+> prettyPrintAst expr
prettyPrintAst (For _ ident start end sts) = "for (var" 
  <> pretty ident <+> "=" <+> prettyPrintAst start <> ";" 
  <+> pretty ident <+> "<" <+> prettyPrintAst end <> ";"
  <+> pretty ident <> "++"
  <+> prettyPrintAst sts
prettyPrintAst (ForIn _ ident obj sts) = "for (var" <+> pretty ident <+> "in" <+> prettyPrintAst obj <> ")" <+> prettyPrintAst sts
prettyPrintAst (IfElse _ cond thens elses) = "if" <+> parens (prettyPrintAst cond) <+> prettyPrintAst thens <> prettyElse elses
    where 
      prettyElse (Just s) = " else" <+> prettyPrintAst s
      prettyElse Nothing = ""
prettyPrintAst (ReturnNoResult _) = "return"
prettyPrintAst (Throw _ value) = "throw" <+> prettyPrintAst value
prettyPrintAst (Comment _ com ast) = sep (map prettyPrintComment com) <> prettyPrintAst ast
prettyPrintAst (InstanceOf _ a b) = prettyPrintAst a <+> "instanceof" <+> prettyPrintAst b

prettyPrintComment :: Comment -> Doc ()
prettyPrintComment (LineComment com) = "//" <+> pretty com <> line
prettyPrintComment (BlockComment com) = "/*" <+> pretty com <+> "*/"

-- prettyPrintAst s = pretty $ show s

prettyPrintSts :: [AST] -> Doc ()
prettyPrintSts sts = cat $ zipWith (<>) (prettyPrintAst <$> sts) (repeat ";")

prettyPrintStsTopLevel :: [AST] -> Doc ()
prettyPrintStsTopLevel sts = cat $ zipWith (<>) (prettyPrintAst <$> sts) (repeat $ ";"<>line')

layoutOptions :: LayoutOptions
layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine 120 0.8 }

prettyPrintJS :: [AST] -> Text
prettyPrintJS ast = renderStrict $ layoutPretty layoutOptions $ prettyPrintStsTopLevel ast -- maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements
