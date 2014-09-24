{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Data.Functor

data Term = S | K | Apply Term Term deriving (Eq)

instance Show Term where
    show S = "S"
    show K = "K"
    show (Apply l r) = "(" ++ show l ++ " " ++ show r ++ ")"

term :: Parser Term
term = skipSpace *> choice [s, k, apply] <* skipSpace
    where
    s     = pure S     <* "S"
    k     = pure K     <* "K"
    apply = pure Apply <* "(" <*> term <*> term <* ")"

main :: IO ()
main = interact (unlines . map process . lines)
    where
    process = either id (show . evaluate) . parseOnly (term <* endOfInput) . pack

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
