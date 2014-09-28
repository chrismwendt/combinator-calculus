import CombinatorCalculus hiding (main)
import Control.Applicative
import Control.Monad
import Data.Either

main :: IO ()
main = do
    forM_ tests $ \(input, expected) -> if runTest input expected
        then putStrLn $ "Passed: " ++ input ++ " -> " ++ expected
        else putStrLn $ "--- Failed: " ++ input ++ " -> " ++ expected
    putStrLn "Finished testing."

runTest :: String -> String -> Bool
runTest input expected = either (const False) id $ do
    term <- parseTerm input
    t2 <- parseTerm expected
    return $ renderTerm (eval term) == renderTerm (eval t2)
    where
    eval = last . take 1000 . evaluate

tests :: [(String, String)]
tests =
    [ ("S", "S")
    , ("K", "K")
    , ("(K K S)", "K")
    , ("(K S S)", "S")
    , ("(S S S S)", "(S S (S S))")
    , ("(K (K K K) K)", "K")
    , ("(K K (S (S K K) (S K K) (S (S K K) (S K K))))", "K") -- const const omega
    , ("(S (K K K) K K)", "K")
    , ("(K (S K) K)", "(S K)") -- T NOT
    , ("(S K (S K) K)", "K") -- F NOT
    , ("(K K K)", "K") -- T OR T
    , ("(K K (S K))", "K") -- T OR F
    , ("(S K K K)", "K") -- F OR T
    , ("(S K K (S K))", "(S K)" ) -- F OR F
    , ("(K K (S K))", "K") -- T T AND
    , ("(K (S K) (S K))", "(S K)") -- T F AND
    , ("(S K K (S K))", "(S K)") -- F T AND
    , ("(S K (S K) (S K))", "(S K)") -- F F AND
    ]
