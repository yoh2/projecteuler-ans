-- 実際に列を作って調べる。
import Data.Char (digitToInt)

answer :: Int
answer = product
         $ map (digitToInt . (string!!))
               [1, 10, 100, 1000, 10000, 100000, 1000000]
         where
           -- !!n で1基点でのn番目の値が取れるように頭にダミーの0を挿入。
           string = '0' : (concat $ map show [1..])

main :: IO ()
main = print answer
