module Parser (Parser (..), ParseResult (..)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))

data ParseResult tkn
  = Success tkn
  | Failed

instance Functor ParseResult where
  fmap f (Success a) = Success $ f a
  fmap _ Failed = Failed

instance Applicative ParseResult where
  pure = Success
  Success f <*> a = f <$> a
  Failed <*> _ = Failed

instance Monad ParseResult where
  Success a >>= f = f a
  Failed >>= _ = Failed

instance Alternative ParseResult where
  empty = Failed
  Failed <|> b = b
  a <|> _ = a

newtype Parser input tkn = Parser {munch :: input -> ParseResult (tkn, input)}

deriving instance Functor (Parser input)

instance Applicative (Parser input) where
  pure a = Parser $ pure . (a,)
  Parser f <*> Parser v = Parser $ f >=> (\(f', i') -> first f' <$> v i')

instance Alternative (Parser input) where
  empty = Parser $ const Failed
  Parser a <|> Parser b = Parser $ \i -> a i <|> b i

instance Monad (Parser input) where
  Parser a >>= f = Parser $ a >=> (\(a', s') -> munch (f a') s')
