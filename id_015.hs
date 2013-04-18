-- the number of routes to pass through N x N grid
-- is combination (2N, N).

combination :: (Integral a) => a -> a -> a
combination n m = product [n - m + 1 .. n] `div` product [1 .. m]
solution :: Integer
solution = combination 40 20

main :: IO ()
main = print solution
