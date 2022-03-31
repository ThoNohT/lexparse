module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Tokenizer

charTokenizer :: Char -> (Char -> Tokenizer tkn) -> Tokenizer tkn
charTokenizer char cont =
  Tokenizer $ \case
    Just ch | ch == char -> Collecting $ cont ch
    _ -> Failed

doneTokenizer :: tkn -> Tokenizer tkn
doneTokenizer res = Tokenizer $ \_ -> Done res

failedTokenizer :: Tokenizer tkn
failedTokenizer = Tokenizer $ const Failed

stringLiteralTokenizer :: Tokenizer Text
stringLiteralTokenizer =
  charTokenizer '"' (\_ -> insideTokenizer "")
 where
  insideTokenizer :: Text -> Tokenizer Text
  insideTokenizer soFar = Tokenizer $ \case
    Just '"' -> Collecting $ doneTokenizer soFar
    Just ch -> Collecting $ insideTokenizer (T.snoc soFar ch)
    Nothing -> Failed

testTokenizers :: [Tokenizer String]
testTokenizers =
  [ charTokenizer 'a' (\c1 -> charTokenizer 'a' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'b' (\c1 -> charTokenizer 'b' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'b' (\c1 -> charTokenizer 'a' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'a' (\c1 -> doneTokenizer [c1])
  , T.unpack <$> stringLiteralTokenizer
  ]

main :: IO ()
main = do
  print $ tokenize testTokenizers "aabbba"
  print $ tokenize testTokenizers "abb\"Hello, World!\"baa"
