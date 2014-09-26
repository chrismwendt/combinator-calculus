{-# LANGUAGE OverloadedStrings #-}

module CombinatorCalculus where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
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

renderTerm :: Term -> String
renderTerm = P.render . toDoc

main :: IO ()
main = interact (unlines . map process . lines)
    where
    process = either id eval . parseTerm . pack
    eval = renderTerm . last . take maxSteps . evaluate
    renderTerm = P.renderStyle (P.style { P.mode = P.OneLineMode }) . toDoc
    parseTerm = parseOnly (term <* endOfInput)
    maxSteps = 1000

evaluate :: Term -> [Term]
evaluate t@S = [t]
evaluate t@K = [t]
evaluate t@(Apply (Apply K a) b) = [t] ++ evaluate a
evaluate t@(Apply (Apply (Apply S a) b) c) = [t] ++ evaluate (Apply (Apply a c) (Apply b c))
evaluate t@(Apply a b) = [t] ++ map (flip Apply b) (tail as) ++ map (Apply a') (tail bs)
    where
    as = evaluate a
    a' = last as
    bs = evaluate b
