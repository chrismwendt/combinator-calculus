{-# LANGUAGE OverloadedStrings #-}

module CombinatorCalculus where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.List
import Data.Maybe
import qualified Text.PrettyPrint as P

data Term = Apply Combinator [Term] deriving (Eq)

data Combinator = S | K deriving (Eq)

term :: Parser Term
term = applyNone <|> parens applySome
    where
    applyNone    = applyTo $ pure []
    applySome    = applyTo $ term `sepBy1` skipSpace
    applyTo args = Apply <$> combinator <* skipSpace <*> args
    combinator   = s <|> k
    s            = S <$ "S"
    k            = K <$ "K"

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

toDoc :: Term -> P.Doc
toDoc (Apply c args)
    | null args = cDoc
    | otherwise = P.parens (P.sep $ cDoc : argsDocs)
    where
    cDoc = P.text $ case c of
        S -> "S"
        K -> "K"
    argsDocs = map (P.nest 1 . toDoc) args

renderTerm :: Term -> String
renderTerm = P.renderStyle P.style { P.mode = P.OneLineMode } . toDoc

evaluate :: Term -> [Term]
evaluate t = t : unfoldr (fmap dup . step) t
    where
    dup a = (a, a)

step :: Term -> Maybe Term
step (Apply K (a : _ : zs))     = Just $ apply a zs
step (Apply S (a : b : c : zs)) = Just $ apply a (c : b `apply` [c] : zs)
step (Apply c zs)
    | any isJust stepZs = Just $ Apply c (zipWith fromMaybe zs stepZs)
    | otherwise         = Nothing
    where
    stepZs = map step zs

apply :: Term -> [Term] -> Term
apply (Apply c as) bs = Apply c (as ++ bs)
