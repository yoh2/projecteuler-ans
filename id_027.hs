import Data.List (maximumBy)

isPrime :: Integral a => a -> Bool
isPrime x | x <= 1    = False
          | otherwise = and $ map ((/= 0) . (mod x))
                            $ takeWhile (\n -> n * n <= x)
                            $ 2:[3,5..]

answer :: Integer
answer = (\(x, y, _) -> x * y)
         $ maximumBy (\(_, _, x) (_, _, y) -> compare x y)
         [(a, b, length $ takeWhile isPrime
                          [n^2 + a * n + b | n <- [0..]])
          | a <- [(-999) .. 999], b <- [0 .. 999]]

main :: IO ()
main = print answer
