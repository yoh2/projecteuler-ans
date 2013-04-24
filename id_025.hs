fibs :: [Integer]
fibs = 1:1:(zipWith (+) fibs $ tail fibs)

-- 条件を満たさない (< 10^999) 要素の配列の長さ
-- = 条件を満たさない最後の要素のインデックス。
-- -> 条件を満たす最初のひとつはそれに1を足したもの。
answer :: Int
answer = (length $ takeWhile (< 10^999) fibs) + 1

main :: IO ()
main = print answer
