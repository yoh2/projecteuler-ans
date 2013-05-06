-- 計算量を減らすなどの工夫を何も考えずに書き下しただけで
-- 待ち時間もなく一瞬で結果が出てしまった。あんまりだ。
-- Haskell恐るべし。
answer :: Integer
answer = (sum $ map (\x -> x ^ x) [1 .. 1000]) `mod` 10000000000

main :: IO ()
main = print answer
