import Data.String (words)

-- n個の整数列 -> n+1個の整数列 -> result!!i = n!!i + max (n!!i) (n!!(i+1))
addMax :: (Integral a) => [a] -> [a] -> [a]
addMax []     _          = []
addMax (x:xs) (y0:y1:ys) = (x + max y0 y1) : addMax xs (y1:ys)

answer :: IO Int
answer = do content <- readFile "triangle.txt"
            let triangleIntList = map ((map read) . words) $ lines content
            return (head $ foldr1 addMax triangleIntList)

main :: IO ()
main = answer >>= print
