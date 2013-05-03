-- 分数モジュールを使うのは楽しすぎ？
-- 使わない場合も、分子と分母のペアを最大公約数で割るだけで
-- 既約分数を得られるので対して手間に差はないと思うが。
import Data.Ratio

answer :: (Integral a) => a
-- 式末尾のリストは xy / yz = x / z, xy / yz < 1 となるような値。
-- 2桁なのでxとyは1以上。x / zも計算可能になるためにはzも1以上。
-- したがってすべての桁が1以上でなければならない。
-- 上記の条件を満たすようにすると、tribialな例 (x0/y0 = x/y) は
-- 自動的に除外される。
answer = denominator $ product [a % b
                                | x <- [1 .. 9],
                                  y <- [x .. 9], -- 明らかにxより小さい値は不要
                                  z <- [1 .. 9],
                                  let a = x * 10 + y,
                                  let b = y * 10 + z,
                                  a < b,
                                  a % b == x % z]

main :: IO ()
main = print answer
