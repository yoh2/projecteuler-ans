import List

isPalindromicNumber :: (Integral a) => a -> Bool
isPalindromicNumber x = digitsr == reverse digitsr
                        where digitsr = splitDigitsR x

splitDigitsR :: (Integral a) => a -> [a]
splitDigitsR 0 = []
splitDigitsR x = mod x 10 : splitDigitsR (div x 10)

solution :: Integer
solution = head $ filter isPalindromicNumber $ sortBy (flip compare) [x * y | x <- [100 .. 999], y <- [100 .. 999]]

main :: IO ()
main = print solution
