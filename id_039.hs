import Data.List (maximumBy)
rightTriangle :: (Integral a) => a -> [(a, a, a)]
rightTriangle p = [(a, b, c) |
                     a <- [1 .. p `div` 3],
                     b <- [a .. (p - a) `div` 2],
                     let c = p - a - b,
                     a * a + b * b == c * c]

answer :: Integer
answer = (\xs -> let (a, b, c) = head xs in a + b + c)
         $ maximumBy (\x y -> compare (length x) (length y))
         $ map rightTriangle [3 .. p]
         where p = 1000

main :: IO ()
main = print answer
