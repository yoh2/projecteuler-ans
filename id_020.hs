import Data.Char (digitToInt)

answer :: Int
answer = sum $ map digitToInt $ show $ product [1 .. 100]

main :: IO ()
main = print answer
