module Main where

import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Text (Text, pack, snoc, unpack)
import Tokenizer
import Tokenizers as T

stringLiteralTokenizer :: Tokenizer Text
stringLiteralTokenizer =
  T.char '"' *> insideTokenizer ""
 where
  insideTokenizer :: Text -> Tokenizer Text
  insideTokenizer soFar = Tokenizer $ \case
    Just '"' -> Collecting $ pure soFar
    Just ch -> Collecting $ insideTokenizer (snoc soFar ch)
    Nothing -> Failed

slt2 :: Tokenizer Text
slt2 = pack <$> (T.char '\'' *> T.many (T.pred ('\'' /=)))

testTokenizers :: [Tokenizer String]
testTokenizers =
  [ T.char 'a' >>= \c1 -> T.char 'a' >>= \c2 -> pure [c1, c2]
  , T.char 'b' >>= \c1 -> T.char 'b' >>= \c2 -> pure [c1, c2]
  , T.char 'b' >>= \c1 -> T.char 'a' >>= \c2 -> pure [c1, c2]
  -- TODO: This should be the same as T.char 'a' <&> singleton but it is not.
  -- This is because pure consumes a token, while <&> simply maps the result. Figure out what can be done about this.
  , T.char 'a' >>= pure . singleton
  , unpack <$> stringLiteralTokenizer
  , unpack <$> slt2
  , T.many (T.pred $ const True) -- TODO: Some other tokenizer consumes the trailing " in the last test, why?
  ]

main :: IO ()
main = do
  print $ tokenize testTokenizers "aabbba"
  print $ tokenize testTokenizers "abb\"Hello, World!\"baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa"
  print $ tokenize testTokenizers "abb'Hello, World!'baa\""
