-- 手抜きしすぎ？
import Data.Set

answer :: Int
answer = size $ fromList [a ^ b | a <- [2 .. 100], b <- [2 .. 100]]

main :: IO ()
main = print answer
