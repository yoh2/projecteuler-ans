-- 整数 → (素因数,冪数)のリスト。
-- この問題を解くだけなら素因数は不要。
factors :: (Integral a, Eq b, Num b) => a -> [(a,b)]
factors n = factorsSub 2 n

factorsSub :: (Integral a, Eq b, Num b) => a -> a -> [(a, b)]
factorsSub _ 1 = []
factorsSub f n | f > n = [(n, 1)]
               | otherwise = case factor f n of
                               (0, _) -> factorsSub (f + 1) n
                               (p, m) -> (f, p) : factorsSub (f + 1) m

-- 因数 → 整数 → (因数の冪数, 整数/因数^冪数)
factor :: (Integral a, Num b) => a -> a -> (b, a)
factor f n | n `mod` f == 0 = let (p, m) = factor f (n `div` f) in (1 + p, m)
           | otherwise      = (0, n)

-- お好みで好きな方を。
triangulars :: [Integer]
triangulars = scanl1 (+) [1..]
--triangulars = map (\x -> x * (x + 1) `div` 2) [1..]

solution :: Integer
solution = head $ dropWhile  ((< 500) . product . (map (succ . snd)) . factors) triangulars

main :: IO ()
main = print solution
