import Data.Set (Set, fromList, (\\), fold)
-- 約数を求める方法はID 21から流用。

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

properDivisorsSum :: (Integral a) => a -> a
properDivisorsSum = sum . tail . reverse . divisors -- divisorsは自分自身が末尾にくるので削ったものを足す。

-- 過剰数のリスト。
abundantNumbers :: (Integral a) => [a]
abundantNumbers = [x | (x, y) <- zip [1..] $ map properDivisorsSum [1..],
                        x < y]

upperLimit = 28123
abundantSums :: [Integer]
abundantSums = let xs = takeWhile (<= upperLimit) abundantNumbers
               in [p + q | p <- xs, q <- xs, p + q <= upperLimit]

abundantSumSet :: Set Integer
abundantSumSet = fromList [1 .. upperLimit] \\ fromList abundantSums

answer :: Integer
answer = fold (+) 0 abundantSumSet

main :: IO ()
main = print answer
