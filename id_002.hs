fibs :: (Integral a) => [a]
fibs = 1:2:(zipWith (+) fibs $ tail fibs)

solution :: Integer
solution = foldl (+) 0 $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<= 4000000) fibs

main :: IO ()
main = print solution
