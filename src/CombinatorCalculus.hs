{-# LANGUAGE OverloadedStrings #-}

module CombinatorCalculus
    (
      Combinator(..)
    , Term
    , parseTerm
    , renderTerm
    , step
    , evaluate
    , apply
    ) where

import Control.Applicative
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.List
import Data.Maybe
import Data.Tree
import qualified Text.PrettyPrint as P

data Combinator = S | K deriving (Eq, Show)

type Term = Tree Combinator

parseTerm :: String -> Either String Term
parseTerm = parseOnly (term <* endOfInput) . pack

term :: Parser Term
term = foldl1 apply <$> subTerm `sepBy1` skipSpace
    where
    subTerm = choice [Node S [] <$ "S", Node K [] <$ "K", parens term]

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

renderTerm :: Term -> String
renderTerm = P.renderStyle P.style { P.mode = P.OneLineMode } . toDoc

toDoc :: Term -> P.Doc
toDoc (Node c args) = P.sep (c' : map toDocInner args)
    where
    toDocInner t@(Node _ []) = toDoc t
    toDocInner t              = P.parens (toDoc t)
    c' = P.text $ case c of
        S -> "S"
        K -> "K"

evaluate :: Term -> [Term]
evaluate t = t : unfoldr (fmap dup . step) t
    where
    dup a = (a, a)

step :: Term -> Maybe Term
step (Node K (a : _ : zs))     = Just $ applyMany a zs
step (Node S (a : b : c : zs)) = Just $ applyMany a (c : b `apply` c : zs)
step (Node c zs)
    | any isJust stepZs = Just $ Node c (zipWith fromMaybe zs stepZs)
    | otherwise         = Nothing
    where
    stepZs = map step zs

apply :: Term -> Term -> Term
apply (Node c as) b = Node c (as ++ [b])

applyMany :: Term -> [Term] -> Term
applyMany (Node c as) bs = Node c (as ++ bs)
