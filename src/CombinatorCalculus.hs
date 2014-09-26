{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Functor
import qualified Text.PrettyPrint as P

data Term = S | K | Apply Term Term deriving (Eq)

term :: Parser Term
term = skipSpace *> choice [s, k, apply] <* skipSpace
    where
    s     = pure S     <* "S"
    k     = pure K     <* "K"
    apply = pure Apply <* "(" <*> term <*> term <* ")"

toDoc :: Term -> P.Doc
toDoc S           = P.text "S"
toDoc K           = P.text "K"
toDoc (Apply l r) = P.parens $ P.sep [P.nest 1 (toDoc l), P.nest 1 (toDoc r)]

main :: IO ()
main = interact (unlines . map process . lines)
    where
    process = either id (renderTerm . evaluate) . parseTerm . pack
    renderTerm = P.renderStyle (P.style { P.mode = P.OneLineMode }) . toDoc
    parseTerm = parseOnly (term <* endOfInput)

step :: Term -> Term
step S = S
step K = K
step (Apply (Apply K a) b) = a
step (Apply (Apply (Apply S a) b) c) = Apply (Apply a c) (Apply b c)
step (Apply a b) = Apply (evaluate a) (evaluate b)

evaluate :: Term -> Term
evaluate term
    | term == step term = term
    | otherwise = evaluate (step term)
