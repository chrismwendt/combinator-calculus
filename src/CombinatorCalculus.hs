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

parseTree :: Parser a -> String -> Either String (Tree a)
parseTree a = parseOnly (tree a <* endOfInput) . pack

tree :: Parser a -> Parser (Tree a)
tree a = foldl1 apply <$> subTree `sepBy1` skipSpace
    where
    subTree = Node <$> a <*> pure [] <|> parens (tree a)

parseTerm :: String -> Either String Term
parseTerm = parseTree (S <$ "S" <|> K <$ "K")

renderTree :: Tree String -> String
renderTree = P.renderStyle P.style { P.mode = P.OneLineMode } . toDoc
    where
    toDoc (Node a forest)  = P.sep (P.text a : map innerToDoc forest)
    innerToDoc (Node a []) = P.text a
    innerToDoc t           = P.parens (toDoc t)

renderTerm :: Term -> String
renderTerm = renderTree . fmap show

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

apply :: Tree a -> Tree a -> Tree a
apply (Node c as) b = Node c (as ++ [b])

applyMany :: Tree a -> [Tree a] -> Tree a
applyMany (Node c as) bs = Node c (as ++ bs)

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"
