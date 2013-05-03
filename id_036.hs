-- メモ
-- 今回の方針: [1, 1000000) の範囲の各値について、基数2の下で回文となっているかと
-- 基数10の下で回文となっているかの判定をしている。
-- これを、まず、指定された範囲で基数10 (または2) の回文を作り、それが
-- 基数2 (または10) の範囲で回文になっているかを判定すると速くなるのではないか?

-- 自然数を基数bの下で各桁に分割する。
intToDigitListInBase :: (Integral a) => a -> a -> [a]
intToDigitListInBase b n = snd $ head $ dropWhile ((> 0) . fst)
                           $ iterate (\(x, xs) -> (x `div` b, x `mod` b : xs))
                                     (n, [])

-- 基数bの下で分割された桁のリストを自然数に戻す。
-- 書いたはいいが今回使わずに済んだ。
digitListInBaseToInt :: (Integral a) => a -> [a] -> a
digitListInBaseToInt b = foldl1 (\x y -> x * b + y)

-- 自然数nが基数bの下で回文となっているか否かを判定する。
isPalindromicInBase :: (Integral a) => a -> a -> Bool
isPalindromicInBase b n = digits == reverse digits
                          where digits = intToDigitListInBase b n

answer :: (Integral a) => a
answer = sum $ filter (\x -> (isPalindromicInBase 2 x) && (isPalindromicInBase 10 x))
                      [1 .. n]
         where
           n = 1000000 - 1

main :: IO ()
main = print answer
