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
    eval term = case splitAt (1 + maxSteps) (evaluate term) of
        (steps, []) -> renderTerm (last steps)
        _  -> "Reached step limit: " ++ show maxSteps
    maxSteps = 1000
