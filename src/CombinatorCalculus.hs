{-# LANGUAGE OverloadedStrings #-}

module CombinatorCalculus
    ( Term(..)
    , Combinator(..)
    , parseTerm
    , renderTerm
    , step
    , evaluate
    , apply) where

import Control.Applicative
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.List
import Data.Maybe
import qualified Text.PrettyPrint as P

data Term = Apply Combinator [Term] deriving (Eq, Show)

data Combinator = S | K deriving (Eq, Show)

parseTerm :: String -> Either String Term
parseTerm = parseOnly (term <* endOfInput) . pack

term :: Parser Term
term = foldl1 apply <$> subTerm `sepBy1` skipSpace
    where
    subTerm = choice [Apply S [] <$ "S", Apply K [] <$ "K", parens term]

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

renderTerm :: Term -> String
renderTerm = P.renderStyle P.style { P.mode = P.OneLineMode } . toDoc

toDoc :: Term -> P.Doc
toDoc (Apply c args) = P.sep (c' : map toDocInner args)
    where
    toDocInner t@(Apply _ []) = toDoc t
    toDocInner t              = P.parens (toDoc t)
    c' = P.text $ case c of
        S -> "S"
        K -> "K"

evaluate :: Term -> [Term]
evaluate t = t : unfoldr (fmap dup . step) t
    where
    dup a = (a, a)

step :: Term -> Maybe Term
step (Apply K (a : _ : zs))     = Just $ applyMany a zs
step (Apply S (a : b : c : zs)) = Just $ applyMany a (c : b `apply` c : zs)
step (Apply c zs)
    | any isJust stepZs = Just $ Apply c (zipWith fromMaybe zs stepZs)
    | otherwise         = Nothing
    where
    stepZs = map step zs

apply :: Term -> Term -> Term
apply (Apply c as) b = Apply c (as ++ [b])

applyMany :: Term -> [Term] -> Term
applyMany (Apply c as) bs = Apply c (as ++ bs)
