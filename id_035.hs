-- 素数の無限リスト。
primes :: [Integer]
primes = 2 :  filter (\x -> and $ map ((/= 0) . (mod x))
                                $ takeWhile (\y -> y * y <= x) primes)
                     [3, 5 ..]

-- 素数判定。
-- 末尾の 2 : [3, 5..] は、代わりにprimesを使うと、除数の数は減るが、
-- primesが除算を内包しているためか、巨大な素数をTrueと判定する場合、
-- かえって遅くなる傾向があるようだ。
isPrime :: Integral a => a -> Bool
isPrime x | x <= 1    = False
          | otherwise = and $ map ((/= 0) . (mod x))
                            $ takeWhile (\y -> y * y <= x) $ 2 : [3, 5..]

-- 元のリストを回転させたリストを得る。
-- 先頭のリスト必ず元のリストになる。
rotations :: [a] -> [[a]]
rotations xs = rotateSub (length xs) xs
               where
                 rotateSub 0 _      = []
                 rotateSub n (x:xs) = (x:xs)
                                      : (rotateSub (n - 1) (xs ++ [x]))

intToDigitList :: Integral a => a -> [a]
intToDigitList x = snd $ head $ dropWhile ((> 0) . fst)
                   $ iterate (\(x, xs) -> (x `div` 10, (x `mod` 10) : xs))
                             (x, [])

digitListToInt :: Integral a => [a] -> a
digitListToInt = foldl1 (\x y -> x * 10 + y)

intRotations :: Integral a => a -> [a]
--intRotations = (map digitListToInt) . rotations . intToDigitList
-- 1の連続だけはローテーションが重複する可能性があるので特別扱い。
-- (ちなみに、1だけからなる素数は、11の次は1111111111111111111まで存在しない。
-- さらにその次は11111111111111111111111で割と近い)
intRotations x = if and $ map (==1) digitList then [x]
                 else map digitListToInt $ rotations digitList
                 where
                   digitList = intToDigitList x

-- ローテーションのリストを作成した時に、先頭が最も小さな数になるリストだけを
-- 判定対象にすることにより、同じローテーションのグループを重複して判定する
-- ことを避けている。
answer :: Int
answer = length
         $ concat
         $ filter (\xs -> and $ map isPrime xs)
         $ filter (\xs -> minimum xs == head xs)
         $ map intRotations targetPrimes
         where
           n = 1000000
           targetPrimes = takeWhile (< n) primes

main :: IO ()
main = print answer
