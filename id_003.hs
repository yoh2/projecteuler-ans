primes :: (Integral a) => a -> [a]
primes x = case repeatDiv x 2 of
             (0, z) -> primesSub z 3
             (_, z) -> 2 : primesSub z 3

primesSub :: (Integral a) => a -> a -> [a]
primesSub 1 _ = []
primesSub x y | x <= y    = [x]
              | otherwise = case repeatDiv x y of
                              (0, z) -> primesSub z (y + 2)
                              (_, z) -> y : primesSub z (y + 2)

repeatDiv :: (Integral a, Integral b) => a -> a -> (b, a)
repeatDiv x y | x `mod` y == 0 = let (count, z) = repeatDiv (x `div` y) y in (count + 1, z)
              | otherwise      = (0, x)


solution :: Integer
solution = maximum $ primes 600851475143

main :: IO ()
main = print solution
