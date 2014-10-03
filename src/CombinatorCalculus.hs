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
import Data.Foldable (foldlM)
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Tree
import qualified Text.PrettyPrint as P

data Combinator = S | K deriving (Eq, Show)

type Term = Tree Combinator

type Definition a = (String, a)

prelude :: String
prelude = concat
    [
      "T = K"
    , ", F = S K"
    , ", NOT = S(S(K S)(S(K K)S))(K K)"
    , ", OR = S(K S)(S(K(S(K S)))(S(K K)))"
    , ", AND = S(S(K S)(S(K(S(K S)))(S(K(S(K(S(K S)))))(S(K S)(S(K K)(S(K S)K))))))(K(K(K(S K K))))"
    ]

halp p = help (prelude ++ p)

help p = do
    p' <- parseOnly program (pack p)
    t <- p2t p'
    t2 <- return $ last $ take 100 $ evaluate t
    return $ renderTerm t2

p2t :: ([Definition (Tree String)], Tree String) -> Either String Term
p2t (ds, ts) = do
    x <- ds'
    resolveTree x ts
    where
    ds' = foldlM addDef [("K", Node K []), ("S", Node S [])] ds
    addDef as (name, a) = resolveTree as a >>= \a' -> return ((name, a') : as)

program :: Parser ([Definition (Tree String)], Tree String)
program = (,) <$> defs (tree identifier) <* skipSpace <*> tree identifier

defs :: Parser a -> Parser [Definition a]
defs a = def a `sepBy` (skipSpace *> "," <* skipSpace) <* ";"

identifier :: Parser String
identifier = many1 (satisfy $ inClass "A-Z")

def :: Parser a -> Parser (Definition a)
def a = (,) <$> identifier <* skipSpace <* "=" <* skipSpace <*> a

resolveDef (name, value) = (name, (resolveTree value))

resolveTree :: [Definition Term] -> Tree String -> Either String Term
resolveTree ds (Node c args) = case lookup c ds of
    Nothing -> Left $ c ++ " is undefined."
    Just c' -> applyMany c' <$> traverse (resolveTree ds) args

-- resolveDefs :: [Definition String] -> [Definition Term]
-- resolveDefs defs = map resolve defs
--     where
--     resolveDef (Definition name value) = Definition name (resolveTree value)

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
