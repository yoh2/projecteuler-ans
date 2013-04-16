-- my_gcd :: (Integral a) => a -> a -> a
-- my_gcd 0 y = y
-- my_gcd x y | x > y     = my_gcd y x
--         | otherwise = my_gcd (y `mod` x) x
-- 
-- my_lcm :: (Integral a) => a -> a -> a
-- my_lcm x y = x * y `div` z
--           where z = my_gcd x y

solution :: (Integral a) => a
solution = foldl lcm 1 [2 .. 20]

main :: IO ()
main = print solution
