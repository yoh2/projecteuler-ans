-- 基数10の下で回文を構成する自然数を最初に列挙し、その中で
-- 基数2の下で回文を構成する自然数を探すようにしたバージョン。
-- 単純に [1..999999]を列挙して基数10の回文かつ基数2の回文であることを
-- 判定するより遥かに速い。

-- 自然数を基数bの下で各桁に分割する。
intToDigitListInBase :: (Integral a) => a -> a -> [a]
intToDigitListInBase b n = snd $ head $ dropWhile ((> 0) . fst)
                           $ iterate (\(x, xs) -> (x `div` b, x `mod` b : xs))
                                     (n, [])

-- 基数bの下で分割された桁のリストを自然数に戻す。
digitListInBaseToInt :: (Integral a) => a -> [a] -> a
digitListInBaseToInt b = foldl1 (\x y -> x * b + y)

-- 自然数nが基数bの下で回文となっているか否かを判定する。
isPalindromicInBase :: (Integral a) => a -> a -> Bool
isPalindromicInBase b n = digits == reverse digits
                          where digits = intToDigitListInBase b n

-- n個の[0..b-1]の組み合わせを生成する。
-- ただし各要素の先頭には0が含まれない。
digitCombinations :: (Integral a) => a -> Int -> [[a]]
digitCombinations _ 0 = [[]]
digitCombinations b n = foldr (\xs yss
                                -> foldr1 (++)
                                   $ map (\ys
                                            -> map (\x -> x:ys) xs)
                                         yss)
                              [[]]
                              $ ([1 .. b - 1] : (take (n - 1) $ repeat [0 .. b - 1]))

-- 基数bの下で2n桁の回文を生成する。
palindromes2NInBase :: (Integral a) => a -> Int -> [[a]]
palindromes2NInBase b n = map (\xs -> xs ++ reverse xs)
                              $ digitCombinations b n

-- 基数bの下で2n+1桁の回文を生成する。
palindromes2N1InBase :: (Integral a) => a -> Int -> [[a]]
palindromes2N1InBase b 0 = map (\x -> [x]) [1 .. b - 1] -- 1桁の場合に[0]が入らないように場合分け
palindromes2N1InBase b n = foldr1 (++)
                           $ map (\m
                                    -> map (\xs -> xs ++ (m : reverse xs))
                                       $ digitCombinations b n)
                                 [0 .. b - 1]

-- 基数bの下で1桁〜n桁の回文を生成する。
palindromesInBase :: (Integral a) => a -> Int -> [[a]]
palindromesInBase b n = foldr1 (++) $ take n
                        $ zipWith ($) (cycle [palindromes2N1InBase b, palindromes2NInBase b])
                                       -- [0, 1, 1, 2, 2, 3, 3, ...]
                                      (foldr1 (++) $ iterate (\(x0:x1:_) -> [x0 + 1, x1 + 1])
                                                             [0, 1])

answer :: (Integral a) => a
answer = sum $ filter (isPalindromicInBase 2)
                      $ map (digitListInBaseToInt 10)
                      $ palindromesInBase 10 n
         where
           n = 6 -- 百万未満 -> 6桁以下

main :: IO ()
main = print answer
