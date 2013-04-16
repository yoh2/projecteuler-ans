solution :: Int
solution = product $ head [[a, b, c] | a <- [1 .. 1000],
                                       b <- [a + 1 .. 1000],
                                       let c = 1000 - a - b,
                                       b < c,
                                       a ^ 2 + b ^ 2 == c ^ 2]

main :: IO ()
main = print solution
