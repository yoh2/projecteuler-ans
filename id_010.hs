import Data.Set (Set, fromList, union, member)

updatePair :: Integer -> Integer -> (Set Integer, [Integer]) -> (Set Integer, [Integer])
updatePair n x (nonPrimes, primes)
  | x `member` nonPrimes = (union nonPrimes $ fromList [x, x * 2 .. n], primes)
  | otherwise            = (union nonPrimes $ fromList [x * 2, x * 3 .. n], x:primes)

-- n以下の素数のリスト
primesUntil :: Integer -> [Integer]
primesUntil n = snd $ foldr (updatePair n) (fromList [], []) (reverse (2:[3, 5 .. n]))

solution :: Integer
solution = sum $ primesUntil (2000000 - 1)

main :: IO ()
main = print solution
