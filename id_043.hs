import Data.List (permutations)

digitListToInt :: Integral a => [a] -> a
digitListToInt = foldl1 (\x y -> x * 10 + y)

subList :: Int -> Int -> [a] -> [a]
subList pos len = (take len) . (drop pos)

subNumberIsDivisible :: Integral a => Int -> a -> [a] -> Bool
subNumberIsDivisible pos d xs
        = (digitListToInt $ subList pos 3 xs) `mod` d == 0

answer :: Integer
answer = sum
         $ map digitListToInt
         $ filter (\xs -> (head xs /= 0) && (predicate xs))
         $ permutations [0..9]
         where
           predicate xs = and $ map (\(ind, d) -> subNumberIsDivisible (ind - 1) d xs)
                                indDenomList
           indDenomList = [(2, 2), (3, 3), (4, 5), (5, 7), (6, 11), (7, 13), (8, 17)]

main :: IO ()
main = print answer
