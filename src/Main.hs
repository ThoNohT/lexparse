module Main where

import Control.Applicative (Alternative ((<|>)))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import GHC.Exts (the)

data TokenizerResult tkn
  = Collecting (Tokenizer tkn)
  | Done tkn
  | Failed

tokenizerResultToDone :: TokenizerResult tkn -> Maybe tkn
tokenizerResultToDone (Done tkn) = Just tkn
tokenizerResultToDone _ = Nothing

tokenizerResultToCollecting :: TokenizerResult tkn -> Maybe (Tokenizer tkn)
tokenizerResultToCollecting (Collecting tn) = Just tn
tokenizerResultToCollecting _ = Nothing

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

{- | Running tokenizers: Whenever we are between tokens, all known tokenizers will be fed with the next character,
  which should lead them to determine if they can create a token from it. Every tokenizer that returns Failed will
  not be considered for the next step. Whenever the first tokenizer returns Done, this token is returned, and the
  process will restart from the current token. If no tokenizers remain at some point, the text cannot be tokenized.
  If multiple tokenizers are done at the same time, then the first one in the list that returns a result is chosen.
-}
runTokenizers :: [Tokenizer tkn] -> Text -> Maybe (tkn, Text)
runTokenizers [] _ = Nothing
runTokenizers tokenizers input =
  case T.uncons input of
    Nothing ->
      tokenizers
        <&> (`munch` Nothing)
        & mapMaybe tokenizerResultToDone
        & listToMaybe
        <&> (,"")
    Just (char, rest) ->
      let activeTokenizers = (`munch` Just char) <$> tokenizers

          done = listToMaybe $ mapMaybe tokenizerResultToDone activeTokenizers

          collecting = mapMaybe tokenizerResultToCollecting activeTokenizers
       in (,input) <$> done <|> runTokenizers collecting rest

charTokenizer :: Char -> (Char -> Tokenizer tkn) -> Tokenizer tkn
charTokenizer char cont =
  Tokenizer $ \case
    Just ch | ch == char -> Collecting $ cont ch
    _ -> Failed

doneTokenizer :: tkn -> Tokenizer tkn
doneTokenizer res = Tokenizer $ \_ -> Done res

testTokenizers :: [Tokenizer String]
testTokenizers =
  [ charTokenizer 'a' (\c1 -> charTokenizer 'a' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'b' (\c1 -> charTokenizer 'b' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'b' (\c1 -> charTokenizer 'a' (\c2 -> doneTokenizer [c1, c2]))
  , charTokenizer 'a' (\c1 -> doneTokenizer [c1])
  ]

tokenize :: Show tkn => [Tokenizer tkn] -> Text -> Maybe [tkn]
tokenize tokenizers input = reverse <$> go [] tokenizers input
 where
  go acc tokenizers "" = Just acc
  go acc tokenizers input =
    case runTokenizers tokenizers input of
      Nothing -> Nothing
      Just (tkn, rest) -> go (Debug.trace (show tkn) tkn : acc) tokenizers (Debug.trace (T.unpack rest) rest)

main :: IO ()
main = do
  print $ tokenize testTokenizers "aabbba"
