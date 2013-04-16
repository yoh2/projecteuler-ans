sum_square_diff :: (Integral a) => a -> a
sum_square_diff x = (sum xs) ^ 2 - (sum $ map (^2) xs)
                     where xs = [1 .. x]

solution :: Integer
solution = sum_square_diff 100

main :: IO ()
main = print solution
