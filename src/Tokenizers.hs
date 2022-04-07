module Tokenizers where

import Prelude hiding (pred)

import Control.Applicative (Alternative (empty))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Parser (ParseResult (..), Parser (Parser))
import Tokenizer

pchar :: Tokenizer Char
pchar = Parser $ \input ->
  case T.uncons input of
    Nothing -> Failed
    Just (ch, rest) -> Success (ch, rest)

{- | A tokenizer that accepts a character given that it satisfies the provided predicate, and continues with the
 provided tokenizer.
-}
pred :: (Char -> Bool) -> Tokenizer Char
pred prd =
  pchar >>= \case
    ch | prd ch -> pure ch
    _ -> empty

{- | A tokenizer that accepts a single specific character, and continues with the provided tokenizer, or end result
 mapping.
-}
char :: Char -> Tokenizer Char
char char = pred (char ==)
