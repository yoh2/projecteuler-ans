import Data.List (find)

nextCoprime :: (Integral a) => [a] -> (a -> a) -> a -> a
nextCoprime [] _             y = y
nextCoprime xs next y | find ((== 0) . (mod y)) xs == Nothing = y
                      | otherwise = nextCoprime xs next (next y)

-- これやたら遅い……
primes :: (Integral a) => [a]
primes = 2:3:[let part = take n primes
              in nextCoprime part (2+) (2 + last part)
              | n <- [2, 3 ..]]

-- 無理矢理こんなので。
nthPrime n | n <= 1 = 2
           | n == 2 = 3
           | otherwise = nthPrimeSub [3, 2] (n - 1)

nthPrimeSub (p:ps) n | n <= 1 = p
                     | otherwise = nthPrimeSub (q:p:ps) (n - 1)
                       where q = nextCoprime (reverse (p:ps)) (2 +) (2 + p)

solution :: Integer
solution = nthPrime 10001

main :: IO ()
main = print solution
