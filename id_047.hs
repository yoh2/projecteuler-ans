import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)

-- 素因数分解はID 21より。

-- yをxで割り切れるだけ割る。結果は (割り切れた数n, x / y^n)。
repeatDiv :: (Integral a, Num b) => a -> a -> (b, a)
repeatDiv x y | y `mod` x == 0 = let (n, m) = repeatDiv x (y `div` x)
                                 in (n + 1, m)
              | otherwise      = (0, y)

-- xを素因数分解する。結果は (素因数, 冪数) のリスト。
factors :: (Integral a, Num b, Eq b) => a -> [(a, b)]
factors 1 = []
factors x = case repeatDiv 2 x of
              (n, 1) -> [(2, n)]
              (0, m) -> factorsSub 3 m
              (n, m) -> (2, n) : factorsSub 3 m

factorsSub :: (Integral a, Num b, Eq b) => a -> a -> [(a, b)]
factorsSub _ 1 = []
factorsSub x y = case repeatDiv x y of
                   (0, m) -> factorsSub (x + 2) m
                   (n, m) -> (x, n) : factorsSub (x + 2) m

-- 連続したn個の要素が条件を満たす最初の要素を得る。
findSeq :: Int -> (a -> Bool) -> [a] -> Maybe a
findSeq n predicate xs
    = let
        ys = map predicate $ take n xs
        revFailedIndex = findIndex not $ reverse ys
      in
        -- 長さが足りなければNothing
        if length ys < n then
          Nothing
        -- 途中で判定が失敗していた場合はその次から再度判定
        else if isJust revFailedIndex then
          findSeq n predicate $ drop (n - fromJust revFailedIndex) xs
        -- 判定成功
        else
          Just (head xs)

-- factorsの結果をtake 5しているのは、素因数の数が多い時に
-- 余計な素因数分解を避けるための小細工。
answer :: Integer
answer = fromJust $ findSeq 4 (\x -> (length $ take 5 $ factors x) == 4) [1..]

main :: IO ()
main = print answer
