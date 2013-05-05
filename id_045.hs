-- 何も考えずに力技で。

orderedIntersection :: Ord a => [a] -> [a] -> [a]
orderedIntersection [] _ = []
orderedIntersection _ [] = []
orderedIntersection (x:xs) (y:ys) | x == y    = x : orderedIntersection xs ys
                                  | x <  y    = orderedIntersection xs (y:ys)
                                  | otherwise = orderedIntersection (x:xs) ys

orderedIntersectionN :: Ord a => [[a]] -> [a]
orderedIntersectionN = foldr1 orderedIntersection

triangle :: Integral a => a -> a
triangle n = n * (n + 1) `div` 2

pentagonal :: Integral a => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Integral a => a -> a
hexagonal n = n * (2 * n - 1)

answer :: Integer
answer = head
         $ dropWhile (<= 40755)
         $ orderedIntersectionN
         $ map (\f -> map f xs) [triangle, pentagonal, hexagonal]
         where
           xs = [1..]

main :: IO ()
main = print answer
