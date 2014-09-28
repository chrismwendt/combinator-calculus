{-# LANGUAGE OverloadedStrings #-}

import CombinatorCalculus
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
process input = either id eval $ parseTerm input
    where
    eval = renderTerm . last . take maxSteps . evaluate
    maxSteps = 1000
