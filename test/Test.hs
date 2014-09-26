import CombinatorCalculus hiding (main)
import Control.Monad

main :: IO ()
main = do
    mapM_ (\(name, pass) -> putStrLn $ (if pass then "Passed: " else "Failed: ") ++ name) tests
    putStrLn "Finished testing."

tests :: [(String, Bool)]
tests =
    [ ("S -> S", eval S == S)
    , ("K -> K", eval K == K)
    , ("K K S -> K", eval (Apply kk K) == K)
    , ("K S S -> S", eval (Apply ks K) == S)
    , ("S S S S -> S S (S S)", eval (Apply (Apply ss S) S) == (Apply ss ss))
    , ("K (K K K) K -> K", eval (Apply (Apply K (Apply kk K)) K) == K)
    , ("K K omega -> K", eval (Apply kk omega) == K)
    , ("S (K K K) K K -> K", eval (Apply (Apply (Apply S (Apply kk K)) K) K) == K)
    , ("T NOT -> F -> S K", eval (not t) == f)
    , ("F NOT -> T -> K", eval (not f) == t)
    , ("T OR T -> T -> K", eval (or t t) == t)
    , ("T OR F -> T -> K", eval (or t f) == t)
    , ("F OR T -> T -> K" , eval (or f t) == t)
    , ("F OR F -> F -> S K", eval (or f f) == f)
    , ("T T AND -> T -> K", eval (and t t) == t)
    , ("T F AND -> F -> S K", eval (and t f) == f)
    , ("F T AND -> F -> S K", eval (and f t) == f)
    , ("F F AND -> F -> S K", eval (and f f) == f)
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
    eval = last . take 1000 . evaluate
