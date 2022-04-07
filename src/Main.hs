module Main where

import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Text (Text, pack, snoc, unpack)
import Tokenizer
import Tokenizers as T
import Control.Applicative (Alternative(many, some))
import Data.Function ((&))

stringLiteralTokenizer :: Tokenizer Text
stringLiteralTokenizer = pack <$> (T.char '"' *> many (T.pred ('"' /=)) <* T.char '"')

stringLiteralTokenizer2 :: Tokenizer Text
stringLiteralTokenizer2 = pack <$> (T.char '\'' *> many (T.pred ('\'' /=)) <* T.char '\'')

testTokenizers :: [Tokenizer (Token String)]
testTokenizers =
  [ keep $ T.char 'a' >>= \c1 -> T.char 'a' >>= \c2 -> pure [c1, c2]
  , discard $ T.char 'b' >>= \c1 -> T.char 'b' >>= \c2 -> pure [c1, c2]
  , keep $ T.char 'b' >>= \c1 -> T.char 'a' >>= \c2 -> pure [c1, c2]
  , keep $ T.char 'a' <&> singleton

  , keep $ unpack <$> stringLiteralTokenizer
  , keep $ unpack <$> stringLiteralTokenizer2
  , keep $ singleton <$> T.pchar
  ]

main :: IO ()
main = do
  print $ tokenize testTokenizers "aabbba"
  print $ tokenize testTokenizers "abb\"Hello, World!\"baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa\"xa"
