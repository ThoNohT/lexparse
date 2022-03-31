module Main where

import Data.List (singleton)
import Data.Text (Text, pack, snoc, unpack)
import Tokenizer
import Tokenizers as T

stringLiteralTokenizer :: Tokenizer Text
stringLiteralTokenizer =
  T.char '"' |/=> insideTokenizer ""
 where
  insideTokenizer :: Text -> Tokenizer Text
  insideTokenizer soFar = Tokenizer $ \case
    Just '"' -> Collecting $ T.done soFar
    Just ch -> Collecting $ insideTokenizer (snoc soFar ch)
    Nothing -> Failed

slt2 :: Tokenizer Text
slt2 = pack <$> T.char '\'' |/=> T.preds ('\'' /=) T.done (const Failed)

testTokenizers :: [Tokenizer String]
testTokenizers =
  [ T.char 'a' |=> \c1 -> T.char 'a' |=> \c2 -> T.done [c1, c2]
  , T.char 'b' |=> \c1 -> T.char 'b' |=> \c2 -> T.done [c1, c2]
  , T.char 'b' |=> \c1 -> T.char 'a' |=> \c2 -> T.done [c1, c2]
  , T.char 'a' |=> (T.done . singleton)
  , unpack <$> stringLiteralTokenizer
  , unpack <$> slt2
  , T.preds (const True) T.done Done
  ]

main :: IO ()
main = do
  print $ tokenize testTokenizers "aabbba"
  print $ tokenize testTokenizers "abb\"Hello, World!\"baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa\""
