isPrime :: (Integral a) => a -> Bool
isPrime x | x <= 1    = False
          | x == 2    = True
          | otherwise = and $ map ((/= 0) . (mod x))
                            $ takeWhile (\n -> n * n <= x)
                            $ 2:[3,5..]

pickup1 :: [a] -> [(a, [a])]
pickup1 []     = error "empty list."
pickup1 [x]    = [(x, [])]
pickup1 (x:xs) = (x, xs) : map (\ (y, ys) -> (y, x:ys))
                                 (pickup1 xs)

-- リストの要素の全ての並び順のパターンを得る。
-- 元のリストの要素がが昇順または降順になっていた場合、
-- 得られたリスト同士の要素の並び順も昇順または降順となる。
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = concat
                   $ map (\ (y, ys) -> map (y:) $ permutations' ys)
                   $ pickup1 xs

digitListToInt :: (Integral a) => [a] -> a
digitListToInt = foldl1 (\x y-> x * 10 + y)

answer :: Integer
answer = head
         $ filter isPrime
         $ concat
         $ map (\xs -> map digitListToInt
                       $ permutations' xs)
               [[n, n - 1 .. 1] | n <- [9, 8 .. 1]]

main :: IO ()
main = print answer
