primes :: [Integer]
primes = 2 : filter (\x -> and $ map ((/= 0) . (mod x))
                               $ takeWhile (\y -> y * y <= x) primes)
                    [3, 5 ..]

answer :: Integer
answer = primes !! (n - 1)
         where n = 10001

main :: IO ()
main = print answer
