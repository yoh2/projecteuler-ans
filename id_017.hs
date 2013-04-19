-- 真面目に英語表現 (単語リスト) を作ってから文字数を数える方針で。
-- 単に文字数のリストを作って足し合わせる方が速いけど。
-- (もっと言うと特定のパターンの繰り返しなのでひとつひとつわざわざ変換する
-- 必要はない。)

-- 20未満の整数の変換
under20Words :: [String]
under20Words = ["zero", "one", "two", "three", "four",
                "five", "six", "seven", "eight", "nine",
                "ten", "eleven", "twelve", "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

under20ToWord :: Int -> String
under20ToWord n = under20Words !! n

-- 10の位
tensPlaceWords :: [String]
tensPlaceWords = ["zero", "ten", "twenty", "thirty", "forty",
                   "fifty", "sixty", "seventy", "eighty", "ninety"]

tensPlaceToWord :: Int -> String
tensPlaceToWord n = tensPlaceWords !! (n `div` 10)

-- 100未満の整数の変換
under100ToWords :: Int -> [String]
under100ToWords n
  | n < 20    = [under20ToWord n]
  | otherwise = case n `mod` 10 of
                  0 -> [tensPlaceToWord n]
                  m -> [tensPlaceToWord n, under20ToWord m]

-- 1000未満の整数の変換
under1000ToWords :: Int -> [String]
under1000ToWords n
  | n < 100   = under100ToWords n
  | otherwise = let
                  ge100Words = [under20ToWord (n `div` 100), "hundred"]
                in
                  case n `mod` 100 of
                    0 -> ge100Words
                    m -> ge100Words ++ "and" : under100ToWords m

-- 1000の変換については、これだけのために10000未満の整数の変換を
-- 作るのが面倒だったので即値で。
-- 単語リストのリスト→単語リスト→文字リスト→文字数
solution :: Int
solution = length $ foldr1 (++) $ foldr1 (++)
           $ map under1000ToWords [1 .. 999] ++ [["one", "thousand"]]

main :: IO ()
main = print solution
