import CombinatorCalculus hiding (main)
import Control.Monad

main :: IO ()
main = do
    mapM_ (\(name, pass) -> putStrLn $ (if pass then "Passed: " else "Failed: ") ++ name) tests
    putStrLn "Finished testing."

tests :: [(String, Bool)]
tests =
    [ ("S -> S", evaluate S == S)
    , ("K -> K", evaluate K == K)
    , ("K K S -> K", evaluate (Apply kk K) == K)
    , ("K S S -> S", evaluate (Apply ks K) == S)
    , ("S S S S -> S S (S S)", evaluate (Apply (Apply ss S) S) == (Apply ss ss))
    , ("K (K K K) K -> K", evaluate (Apply (Apply K (Apply kk K)) K) == K)
    , ("K K omega -> K", evaluate (Apply kk omega) == K)
    ]
    where
    kk = Apply K K
    ks = Apply K S
    ss = Apply S S
    sk = Apply S K
    i = Apply sk K
    sii = Apply (Apply S i) i
    omega = Apply sii sii
