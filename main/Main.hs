{-# LANGUAGE OverloadedStrings #-}

import CombinatorCalculus
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (pack)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
    where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "λ "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                outputStrLn $ process input
                loop

process :: String -> String
process input = either id eval $ parseTerm (pack input)
    where
    parseTerm = parseOnly (term <* endOfInput)
    eval = renderTerm . last . take maxSteps . evaluate
    maxSteps = 1000
