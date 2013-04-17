import Data.List (find)

nextCoprime :: (Integral a) => [a] -> (a -> a) -> a -> a
nextCoprime [] _             y = y
-- xsが降順の時に速くなりやすい検索。
nextCoprime xs next y =
  if (find ((== 0) . (mod y)) $ reverse xs) == Nothing then y
  else nextCoprime xs next (next y)

-- primeListList !! n = 小さい方からn個の素数が降順に並んだリスト
primeListList :: [[Integer]]
primeListList = []:[2]:(iterate (\xs -> (nextCoprime xs (2 + ) (2 + head xs)) : xs) [3, 2])

solution :: Integer
solution = head $ primeListList !! (10001)

main :: IO ()
main = print solution
