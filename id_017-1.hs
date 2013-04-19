-- 1 .. 99 の内訳
-- 0は今回0文字として数える。
-- "one" .. "nine" が9回ずつ (11 .. 19は出現しないので9回になる)
-- "eleven" .. "nineteen" が1回ずつ
-- "ten" が1回
-- "twenty" .. "ninety" が10回ずつ

charSum :: [String] -> Int
charSum = foldr ((+).length) 0

charSum1To9 :: Int
charSum1To9 = charSum ["one", "two", "three", "four", "five",
                       "six", "seven", "enght", "nine"]
charSum20To90 :: Int
charSum20To90 = charSum ["twenty", "thirty", "forty", "fifty",
                         "sixty", "seventy", "eighty", "ninety"]

charSum11To19 :: Int
charSum11To19 = charSum ["eleven", "twelve", "thirteen", "fourteen", "fifteen",
                         "sixteen", "seventeen", "eighteen", "nineteen"]

charSum1To99 :: Int
charSum1To99 = charSum1To9 * 9 + charSum11To19 + length "ten"
               + charSum20To90 * 10

-- 1 .. 999の内訳:
--   1 .. 99 部が10回。
--   "hundred" が 100 .. 999 の 900回。
--   "and" が 100 .. 999 のうち、下位2桁が00を覗き、900 - 9 = 891回。
--   100の位で、"one" .. "nine"が100回ずつ。

charSum1To999 :: Int
charSum1To999 = charSum1To99 * 10
                + length "hundred" * 900
                + length "and" * 891
                + charSum1To9 * 100

solution :: Int
solution = charSum1To999 + length "one" + length "thousand"

main :: IO ()
main = print solution
