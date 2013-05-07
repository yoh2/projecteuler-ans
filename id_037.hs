-- 素数か否かの判定
isPrime :: Integral a => a -> Bool
isPrime x | x <= 1 = False
          | x == 2 = True
          | otherwise = and $ map ((/= 0) . (mod x))
                            $ takeWhile (\n -> n * n <= x)
                            $ 2:[3, 5 ..]

-- 右の桁を削っていってもすべて素数となる素数 (ここでは
-- 右切り詰め可能素数と呼ぶ) のリストのリスト。
-- 先頭から順に 1桁, 2桁, ... の右切り詰め可能素数の
-- リストとなる。
-- また、素数のリストには素数が昇順で含まれる。
--
-- n+1桁の右切り詰め可能素数の全集合は、n桁の右切り詰め可能
-- 素数の全集合のそれぞれの末尾に1桁 (0 ... 9) を付け足した自然数の
-- うち素数であるものの集合となる。
-- 偶数と5は付け足すとかならず合成数となるので、実際には
-- 1, 3, 7, 9を付け足したものの集合から素数を探せばよい。
rightTruncatables :: [[Integer]]
rightTruncatables = iterate (\xs -> [z | x <- xs, y <- [1, 3, 7, 9],
                                         let z = x * 10 + y,
                                         isPrime z]) [2, 3, 5, 7]


-- 左の桁を削っていってもすべて素数となる素数 (ここでは
-- 左切り詰め可能素数と呼ぶ) のリストのリスト。
-- 後述の通り、本問題に合わせた枝刈りをしているため
-- 完全なリストではない。
-- 先頭から順に1桁, 2桁, ... の左切り詰め可能素数のリストとなる。
-- また、素数のリストには素数が昇順で含まれる。
--
-- n+1桁の左切り詰め可能素数の全集合は、1 ... n桁の左切り詰め
-- 可能素数の全集合のそれぞれの先頭に1桁 (1 ... 9; n - 1桁以下の数に
-- 桁を付ける場合は、n+1桁になるように間に0を補完する) 付け足した
-- 自然数のうち素数であるものの集合となる。
-- 
-- ただし、本問題は、左右両方向において切り詰め可能である
-- 素数の集合を求めるものなので、明らかに右切り詰め可能とならない
-- 素数は除外できる。
-- 先頭に2を除く偶数が付加された場合やn-1桁以下の数に桁を追加し0が補完
-- された場合は、その付加された桁がどこにあっても、右切り詰めの過程で、
-- その桁が末尾になった時点で合成数になるので除外できる。
-- また、2の付与については、それが先頭にある場合は右切り詰め可能な
-- 可能性が残るが、途中の桁に2が表れた場合は右切り詰めの過程で
-- その桁右端になると合成数になるため、これも除外できる。
-- 前者の枝刈りはかなりの効果を挙げるが、後者については
-- 問題を解く際の高速化にはあまり寄与していない。
-- ただし、leftTruncatables単体の列挙の高速化の効果は確認できる。
leftTruncatables :: [[Integer]]
leftTruncatables = map snd
                   $ iterate (\(n, xs) -> (n * 10,
                                           [z | y <- [1, 2, 3, 5, 7, 9],
                                                x <- filter (\a -> a < 2 * n || a >= 3 * n) xs,
                                                let z = y * n * 10 + x,
                                                isPrime z]))
                             (1, [2, 3, 5, 7])

-- ふたつの昇順に並んだリストの積集合を取る。
intersectOrderedList :: Ord a => [a] -> [a] -> [a]
intersectOrderedList [] _ = []
intersectOrderedList _ [] = []
intersectOrderedList (x:xs) (y:ys) | x == y    = x : intersectOrderedList xs ys
                                   | x <  y    = intersectOrderedList xs (y:ys)
                                   | otherwise = intersectOrderedList (x:xs) ys

-- 左右両方向に切り詰め可能な素数 (以下、単に切り詰め可能素数と呼ぶ) の
-- リストのリスト。
-- 先頭から順に1桁, 2桁, ... の切り詰め可能素数のリストとなる。
--
-- それぞれのリストの要素は、各方向に切り詰め可能な素数の積集合。
truncatables :: [[Integer]]
truncatables = zipWith intersectOrderedList leftTruncatables rightTruncatables

answer :: Integer
                                  -- 1桁のものを含めない
answer = sum $ take 11 $ concat $ tail truncatables

main :: IO ()
main = print answer
