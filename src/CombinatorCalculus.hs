{-# LANGUAGE OverloadedStrings #-}

module CombinatorCalculus
    (
      Term(..)
    , parseTerm
    , renderTerm
    , step
    , evaluate
    )
    where

import Control.Applicative
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.List
import Data.Maybe
import qualified Text.PrettyPrint as P

data Term = S | K | App Term Term deriving (Eq, Show)

parseTerm :: String -> Either String Term
parseTerm = parseOnly (term <* endOfInput) . pack

term :: Parser Term
term = foldl1 App <$> subTerm `sepBy1` skipSpace
    where
    subTerm = choice [S <$ "S", K <$ "K", parens term]

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

renderTerm :: Term -> String
renderTerm = P.renderStyle P.style { P.mode = P.OneLineMode } . toDoc

toDoc :: Term -> P.Doc
toDoc S                   = P.text "S"
toDoc K                   = P.text "K"
toDoc (App a b@(App _ _)) = P.sep [toDoc a, P.parens (toDoc b)]
toDoc (App a b)           = P.sep [toDoc a, toDoc b]

evaluate :: Term -> [Term]
evaluate t = t : unfoldr (fmap dup . step) t
    where
    dup a = (a, a)

step :: Term -> Maybe Term
step S = Nothing
step K = Nothing
step (App (App K a) _) = Just a
step (App (App (App S a) b) c) = Just (App (App a c) (App b c))
step (App a b)
    | any isJust [a', b'] = Just (App (fromMaybe a a') (fromMaybe b b'))
    | otherwise           = Nothing
    where
    a' = step a
    b' = step b
