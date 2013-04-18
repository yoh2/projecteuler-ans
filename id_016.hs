import Data.Char (digitToInt)

solution :: Int
solution = sum $ map digitToInt  $ show $ 2 ^ 1000

main :: IO ()
main = print solution
