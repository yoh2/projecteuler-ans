primes :: [Integer]
primes = 2 : filter (\x -> and $ map ((/= 0) . (mod x))
                               $ takeWhile (\y -> y * y <= x) primes)
                    [3, 5 ..]

answer :: Integer
answer = sum $ takeWhile (< n) primes
         where n = 2000000

main :: IO ()
main = print answer
