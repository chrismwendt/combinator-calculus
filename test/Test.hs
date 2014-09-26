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
    , ("T NOT -> F -> S K", evaluate (not t) == f)
    , ("F NOT -> T -> K", evaluate (not f) == t)
    , ("T OR T -> T -> K", evaluate (or t t) == t)
    , ("T OR F -> T -> K", evaluate (or t f) == t)
    , ("F OR T -> T -> K" , evaluate (or f t) == t)
    , ("F OR F -> F -> S K", evaluate (or f f) == f)
    , ("T T AND -> T -> K", evaluate (and t t) == t)
    , ("T F AND -> F -> S K", evaluate (and t f) == f)
    , ("F T AND -> F -> S K", evaluate (and f t) == f)
    , ("F F AND -> F -> S K", evaluate (and f f) == f)
    ]
    where
    kk = Apply K K
    ks = Apply K S
    ss = Apply S S
    sk = Apply S K
    i = Apply sk K
    sii = Apply (Apply S i) i
    omega = Apply sii sii
    t = K
    f = sk
    not x = Apply (Apply x f) t
    or a b = Apply (Apply a t) b
    and a b = Apply (Apply a b) f
