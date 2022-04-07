module Tokenizer (Tokenizer, Token, tokenize, keep, discard) where

import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (asum)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Parser (ParseResult (Failed, Success), Parser (munch))

type Tokenizer = Parser Text

data Token tkn = Keep tkn | Discard tkn

{- | Specify that the result of a tokenizer should be kept. -}
keep :: Tokenizer tkn -> Tokenizer (Token tkn)
keep tn = Keep <$> tn

{- | Specify that the result of a tokenizer should be discarded. -}
discard :: Tokenizer tkn -> Tokenizer (Token tkn)
discard tn = Discard <$> tn

toKeep :: Token tkn -> Maybe tkn
toKeep (Keep tkn) = Just tkn
toKeep _ = Nothing

-- | Run the provided tokenizers on the input text, until there is no text remaining, or until all tokenizers fail.
tokenize :: Show tkn => [Tokenizer (Token tkn)] -> Text -> ([tkn], Text)
tokenize tokenizers input = first (mapMaybe toKeep) $ go [] input
 where
  go acc "" = (reverse acc, "")
  go acc input =
    case asum tokenizers `munch` input of
      Success (tkn, rest) -> go (tkn : acc) rest
      Failed -> (reverse acc, input)
