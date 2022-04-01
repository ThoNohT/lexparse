module Tokenizers where

import Prelude hiding (pred)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Tokenizer

{- | A tokenizer that accepts a character given that it satisfies the provided predicate, and continues with the
 provided tokenizer.
-}
pred :: (Char -> Bool) -> Tokenizer Char
pred prd =
  Tokenizer $ \case
    Just ch | prd ch -> Done ch
    _ -> Failed

{- | A tokenizer that accepts a single specific character, and continues with the provided tokenizer, or end result
 mapping.
-}
char :: Char -> Tokenizer Char
char char = pred (char ==)

{- | A tokenizer that accepts a tokenizer and applies it zero or more times until it fails.
 Note that the characters causing a tokenizer to fail are also consumed.
-}
many :: Tokenizer tkn -> Tokenizer [tkn]
many (Tokenizer f) = reverse <$> go []
 where
  go acc = Tokenizer $ \c ->
    case f c of
      Failed -> Done acc
      Done r -> Collecting $ go (r : acc)
      Collecting tn -> Collecting $ go acc

-- | A tokenizer that accepts a tokenizer and applies it until it fails, but fails if it does not succeed at least once.
many1 :: Tokenizer tkn -> Tokenizer [tkn]
many1 tk = do
  first <- tk
  rest <- many tk
  pure $ first : rest
