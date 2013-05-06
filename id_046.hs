import Data.List (find)
import Data.Maybe (isNothing, fromJust)

isPrime :: Integral a => a -> Bool
isPrime x | x <= 1    = False
          | otherwise = and $ map ((/= 0) . (mod x))
                            $ takeWhile (\n -> n * n <= x)
                            $ 2 : [3, 5 ..]
-- 平方の2倍のリスト。
twiceSquares :: Integral a => [a]
twiceSquares = map (\x -> 2 * x * x) [1..]

-- 奇数の合成数のリスト。
oddCompositeNumbers :: Integral a => [a]
oddCompositeNumbers = filter (not . isPrime) [3,5..]

-- 自然数を素数と平方の2倍の和に分解する。
-- できなければNothing
toPrimeAndTwiceSquare :: Integral a => a -> Maybe (a, a)
toPrimeAndTwiceSquare x
    = case find (\twSq -> isPrime $ x - twSq)
           $ takeWhile (< x) twiceSquares
      of
        Just twSq -> Just (x - twSq, twSq)
        Nothing   -> Nothing

answer :: Integer
answer = fromJust $ find (isNothing . toPrimeAndTwiceSquare)
                         oddCompositeNumbers

main :: IO ()
main = print answer
