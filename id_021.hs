-- 素因数分解ならID 12でやってるけど、あえて実装の仕方を変えてみた。
-- とはいえ、基本的な考え方は一緒。

-- yをxで割り切れるだけ割る。結果は (割り切れた数n, x / y^n)。
repeatDiv :: (Integral a, Num b) => a -> a -> (b, a)
repeatDiv x y | y `mod` x == 0 = let (n, m) = repeatDiv x (y `div` x)
                                 in (n + 1, m)
              | otherwise      = (0, y)

-- xを素因数分解する。結果は (素因数, 冪数) のリスト。
factors :: (Integral a, Num b, Eq b) => a -> [(a, b)]
factors 1 = []
factors x = case repeatDiv 2 x of
              (n, 1) -> [(2, n)]
              (0, m) -> factorsSub 3 m
              (n, m) -> (2, n) : factorsSub 3 m

factorsSub :: (Integral a, Num b, Eq b) => a -> a -> [(a, b)]
factorsSub _ 1 = []
factorsSub x y = case repeatDiv x y of
                   (0, m) -> factorsSub (x + 2) m
                   (n, m) -> (x, n) : factorsSub (x + 2) m

-- 素因数分解したリストから全約数を得る。
divisorsFromFactors :: (Integral a) => [(a, Int)] -> [a]
divisorsFromFactors []            = [1]
divisorsFromFactors ((n, m) : xs) = [p * q | p <- take (m + 1) $ iterate (*n) 1,
                                             q <- divisorsFromFactors xs]

divisors :: (Integral a) => a -> [a]
divisors = divisorsFromFactors . factors

-- ここから本題
d :: (Integral a) => a -> a
d = sum . tail . reverse . divisors -- divisorsは自分自身が末尾にくるので削ったものを足す。

amicableNumbers :: (Integral a) => [(a, a)]
amicableNumbers = [(x, y) | x <- [1 .. 10000 - 1], let y = d x, (x < y) && (y < 10000) && (d y == x)]

answer :: (Integral a) => a
answer = foldr (\(x, y) z -> x + y + z) 0 amicableNumbers

main :: IO ()
main = print answer
