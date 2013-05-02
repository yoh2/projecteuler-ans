import Data.List (maximumBy)

isPrime :: (Integral a) => a -> Bool
isPrime n | n <= 1         = False
          | n == 2         = True
          | n `mod` 2 == 0 = False
          | otherwise      = isPrimeSub n 3

isPrimeSub :: (Integral a) => a -> a -> Bool
isPrimeSub x y | x < y * 2      = True
               | x `mod` y == 0 = False
               | otherwise      = isPrimeSub x (y + 2)

answer :: Integer
answer = (\(x, y, _) -> x * y)
         $ maximumBy (\(_, _, x) (_, _, y) -> compare x y)
         [(a, b, length $ takeWhile isPrime
                          [n^2 + a * n + b | n <- [0..]])
          | a <- [(-999) .. 999], b <- [0 .. 999]]

main :: IO ()
main = print answer
