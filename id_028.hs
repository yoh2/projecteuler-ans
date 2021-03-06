-- 筆算で十分解ける。
-- というか筆算で解いて正解した後にプログラム書いた。
--
-- n x n の4隅の値は
--
--   右上: n^2
--   左上: n^2 -  (n - 1)
--   左下: n^2 - 2(n - 1)
--   右下: n^2 - 3(n - 1)
--
-- なので、4隅の合計Cnは
--
--   Cn = 4n^2 - 6(n - 1)
--   (ただしこの計算では C1 = 4になる)
--
-- 対角線の値の合計Dn (nは奇数) は
--   Dn = C1 + C3 + C5 + ... + C(n-2) + Cn - 3
--   (-3 は、C1 = 4となる結果の補正用)
--
-- ここからさらにまとめると筆算で計算できるが、
-- この回答ではCiを素直に足している。
answer :: Integer
answer = (sum $ map (\x -> 4 * x ^ 2 - 6 * (x - 1)) [1, 3 .. 1001]) - 3

main :: IO ()
main = print answer
