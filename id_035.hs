-- 素数リスト生成はID 10 より。
-- ちょくちょく見掛ける
--   primes = 2 : sieve [3, 5..]
--   sieve (p:ps) = p : sieve [q | q <- ps, q `mod` p /= 0]
-- という実装より速いので。

import Data.Set (Set, fromList, union, member)

updatePair :: Integer -> Integer -> (Set Integer, [Integer]) -> (Set Integer, [Integer])
updatePair n x (nonPrimes, primes)
  | x `member` nonPrimes = (union nonPrimes $ fromList [x, x * 2 .. n], primes)
  | otherwise            = (union nonPrimes $ fromList [x * 2, x * 3 .. n], x:primes)

-- n以下の素数のリスト
primesUntil :: Integer -> [Integer]
primesUntil n = snd $ foldr (updatePair n) (fromList [], []) (reverse (2:[3, 5 .. n]))

rotations :: [a] -> [[a]]
rotations xs = rotateSub (length xs) xs
               where
                 rotateSub 0 _      = []
                 rotateSub n (x:xs) = (x:xs)
                                      : (rotateSub (n - 1) (xs ++ [x]))

intToDigitList :: (Integral a) => a -> [a]
intToDigitList x = snd $ head $ dropWhile ((> 0) . fst)
                   $ iterate (\(x, xs) -> (x `div` 10, (x `mod` 10) : xs))
                             (x, [])

digitListToInt :: (Integral a) => [a] -> a
digitListToInt = foldl1 (\x y -> x * 10 + y)

intRotations :: (Integral a) => a -> [a]
intRotations = (map digitListToInt) . rotations . intToDigitList

-- 手抜きのため、ひとつのローテーションについて何度も判定しています。
-- 例えば、199, 919, 991について、それぞれについて[199, 919, 991]が
-- すべて素数リストに含まれるか否か3重に判定しています。
-- でも割と速いので別に問題ないかと。
answer = length
         $ filter (\x -> and $ map (flip member primesSet) $ intRotations x)
                  primes
         where
           n = 1000000
           primes = primesUntil (n - 1)
           primesSet = fromList primes

main :: IO ()
main = print answer
