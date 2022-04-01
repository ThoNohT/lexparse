module Tokenizer (TokenizerResult (..), Tokenizer (..), tokenize) where

import Control.Applicative (Alternative ((<|>)))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text, uncons)

data TokenizerResult tkn
  = Collecting (Tokenizer tkn)
  | Done tkn
  | Failed

instance Functor TokenizerResult where
  fmap f (Collecting tn) = Collecting (f <$> tn)
  fmap f (Done tkn) = Done (f tkn)
  fmap _ Failed = Failed

toDone :: TokenizerResult tkn -> Maybe tkn
toDone (Done tkn) = Just tkn
toDone _ = Nothing

toCollecting :: TokenizerResult tkn -> Maybe (Tokenizer tkn)
toCollecting (Collecting tn) = Just tn
toCollecting _ = Nothing

{- | A Tokenizer takes a character, and then can recursively return a new tokenizer,
 until the result or failure is achieved.
 If success or failure is returned, the current token should not be consumed. Since we don't know if a tokenizer
 failed/succeeded because it stopped recognizing characters, or because it recognized the last character.
 We make the convention that it always only returns Done or Failed if it has seen the first character not part of
 its token.
 When None is provided to a Tokenizer, it should always return Done or Failed, since this means the end of the input.
 Returning Collecting is considered Failed at this point.
-}
newtype Tokenizer tkn = Tokenizer {munch :: Maybe Char -> TokenizerResult tkn}

instance Functor Tokenizer where fmap f (Tokenizer munch) = Tokenizer (fmap f . munch)

instance Applicative Tokenizer where
  pure x = Tokenizer $ \_ -> Done x
  tf <*> tv = applyF tf tv
   where
    applyF :: Tokenizer (a -> b) -> Tokenizer a -> Tokenizer b
    applyF (Tokenizer f) tv@(Tokenizer v) = Tokenizer $ \c ->
      case f c of
        Failed -> Failed
        Collecting tn -> Collecting $ applyF tn tv
        Done f' ->
          Collecting $
            Tokenizer $ \c' ->
              case v c' of
                Failed -> Failed
                Collecting tn -> Collecting $ applyV f' tn
                Done v' -> Done $ f' v'

    applyV :: (a -> b) -> Tokenizer a -> Tokenizer b
    applyV f tv@(Tokenizer v) = Tokenizer $ \c ->
      case v c of
        Failed -> Failed
        Collecting tn -> Collecting $ applyV f tn
        Done v' -> Done $ f v'

instance Monad Tokenizer where
  tv >>= ft = bindV tv ft
   where
    bindV :: Tokenizer a -> (a -> Tokenizer b) -> Tokenizer b
    bindV (Tokenizer v) ft = Tokenizer $ \c ->
      case v c of
        Failed -> Failed
        Collecting tn -> Collecting $ bindV tn ft
        Done v' -> Collecting $
          Tokenizer $ \c' ->
            case ft v' `munch` c' of
              Failed -> Failed
              Collecting tn -> Collecting $ bindF tn
              Done v' -> Done v'

    bindF :: Tokenizer b -> Tokenizer b
    bindF (Tokenizer f) = Tokenizer $ \c ->
      case f c of
        Failed -> Failed
        Collecting tn -> Collecting $ bindF tn
        Done v' -> Done v'

-- instance Monad Tokenizer where

{- | Running tokenizers: Whenever we are between tokens, all known tokenizers will be fed with the next character,
  which should lead them to determine if they can create a token from it. Every tokenizer that returns Failed will
  not be considered for the next step. Whenever the first tokenizer returns Done, this token is returned, and the
  process will restart from the current token. If no tokenizers remain at some point, the text cannot be tokenized.
  If multiple tokenizers are done at the same time, then the first one in the list that returns a result is chosen.
-}
runTokenizers :: [Tokenizer tkn] -> Text -> Maybe (tkn, Text)
runTokenizers [] _ = Nothing
runTokenizers tokenizers input =
  case uncons input of
    Nothing ->
      tokenizers
        <&> (`munch` Nothing)
        & mapMaybe toDone
        & listToMaybe
        <&> (,"")
    Just (char, rest) ->
      let activeTokenizers = (`munch` Just char) <$> tokenizers

          done = listToMaybe $ mapMaybe toDone activeTokenizers

          collecting = mapMaybe toCollecting activeTokenizers
       in (,input) <$> done <|> runTokenizers collecting rest

-- | Run the provided tokenizers on the input text, until there is no text remaining, or until all tokenizers fail.
tokenize :: Show tkn => [Tokenizer tkn] -> Text -> Maybe [tkn]
tokenize tokenizers input = reverse <$> go [] tokenizers input
 where
  go acc tokenizers "" = Just acc
  go acc tokenizers input =
    case runTokenizers tokenizers input of
      Nothing -> Nothing
      Just (tkn, rest) -> go (tkn : acc) tokenizers rest
