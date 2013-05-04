import Data.List (permutations, sortBy, find)

isPrime :: (Integral a) => a -> Bool
isPrime x | x <= 1    = False
          | x == 2    = True
          | otherwise = and $ map ((/= 0) . (mod x)) (2 : [3, 5 .. x `div` 2])

digitListToInt :: (Integral a) => [a] -> a
digitListToInt = foldl1 (\x y-> x * 10 + y)

answer :: Integer
answer = head
         $ filter isPrime
         $ concat
         $ map (\xs -> sortBy (flip compare)
                       $ map digitListToInt
                       $ permutations xs)
               [[1 .. n] | n <- [9, 8 .. 1]]

main :: IO ()
main = print answer
