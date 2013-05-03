-- 力技で。

-- 指定された重みリストの値を用いて指定された値を構成する全パターンを得る。
sumPatterns :: (Num a, Ord a) => [a] -> a -> [[a]]
-- 重みリストが空の場合は構成不能。(たとえamount==0でも構成不能扱いにする)
sumPatterns []     _ = []
                           -- amount が負の値になるパターンは構成不能。
sumPatterns (w:ws) amount | amount <  0 = []
                           -- amount がゼロになるパターンは、空パターンの一種類。
                         | amount == 0 = [[]]
                                         -- 先頭の重みリストをひとつ使った上で構成できるパターン
                         | otherwise   = (map (w:) $ sumPatterns (w:ws) (amount - w))
                                         -- 先頭の重みをもはや使わないパターン
                                         ++ sumPatterns ws amount

-- sumPatterns のパターン数だけ返すバージョン。
sumPatternCount :: (Num a, Ord a) => [a] -> a -> Int
sumPatternCount []     _ = 0
sumPatternCount (w:ws) amount | amount <  0 = 0
                              | amount == 0 = 1
                              | otherwise   = sumPatternCount (w:ws) (amount - w)
                                              + sumPatternCount ws amount

-- 今回は (sumPatternCountではなく) sumPatternsを使う。
answer :: Int
answer = length $ sumPatterns coins amount
         where
           coins = [1, 2, 5, 10, 20, 50, 100, 200]
           amount = 200

main :: IO ()
main = print answer
