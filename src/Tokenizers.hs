module Tokenizers where

import Prelude hiding (pred)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Tokenizer

{- | A tokenizer that accepts a character given that it satisfies the provided predicate, and continues with the
 provided tokenizer.
-}
pred :: (Char -> Bool) -> (Char -> Tokenizer tkn) -> Tokenizer tkn
pred prd cont =
  Tokenizer $ \case
    Just ch | prd ch -> Collecting $ cont ch
    _ -> Failed

{- | A tokenizer that accepts multiple characters as long as it satisfies the predicate, and continues with the
  provided tokenizer once it no longer does. When the end is reached, a contiuation is no longer possible, so
  a function mapping the collected characters to a result is invoked instead.
-}
preds :: (Char -> Bool) -> ([Char] -> Tokenizer tkn) -> ([Char] -> TokenizerResult tkn) -> Tokenizer tkn
preds prd cont atEnd = go []
 where
  go acc = Tokenizer $ \case
    Just ch | prd ch -> Collecting $ go (ch : acc)
    Just _ -> Collecting $ cont $ reverse acc
    Nothing -> atEnd $ reverse acc

-- | Like preds, except it must match at least once.
preds1 :: (Char -> Bool) -> ([Char] -> Tokenizer tkn) -> ([Char] -> TokenizerResult tkn) -> Tokenizer tkn
preds1 prd cont atEnd = pred prd |=> \c -> preds prd (\cs -> cont $ c : cs) (\cs -> atEnd $ c : cs)

{- | A tokenizer that accepts a single specific character, and continues with the provided tokenizer, or end result
 mapping.
-}
char :: Char -> (Char -> Tokenizer tkn) -> Tokenizer tkn
char char = pred (char ==)

-- | A tokenizer that ignores its input and returns Done with the provided value.
done :: tkn -> Tokenizer tkn
done res = Tokenizer $ \_ -> Done res

-- | A tokenizer that ignores its input and returns Failed.
failed :: Tokenizer tkn
failed = Tokenizer $ const Failed
