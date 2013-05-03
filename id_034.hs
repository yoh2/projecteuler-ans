-- 一部 ID30より。

-- 以下のコメントで、自然数を [1の位, 10の位, ...] といった形で表したもの
-- (末尾は非ゼロ) を自然数の桁リスト表現と呼ぶ。

-- 次の数の桁リスト表現を得る。
succDigitList :: (Integral a) => [a] -> [a]
succDigitList []     = [1]
succDigitList (9:xs) = 0 : succDigitList xs
succDigitList (x:xs) = succ x : xs

-- 自然数の桁リスト表現を [0, 1] (10; 2桁の最小の数に相当)
-- から昇順に並んでいる無限リスト。
multiDigitLists :: (Integral a) => [[a]]
multiDigitLists = iterate succDigitList [0, 1]


-- 指定した数以下の自然数の桁リスト表現を得る。
-- ただし最小値は [0, 1] (10)。
multiDigitListsEqOrUnder :: (Integral a) => Int -> [[a]]
multiDigitListsEqOrUnder x = take (x - 9) multiDigitLists

-- 0! .. 9! のリスト。
-- !! n で n! が得られる。
factorials :: [Int]
factorials = map (\x -> product [1 .. x]) [0 .. 9]

-- 桁リスト表現の自然数と、その各桁の階乗の和が等しければ
-- その自然数 (通常の表現) を返し、等しくなければNothingを返す。
selfDigitFactrial :: [Int] -> Maybe Int
selfDigitFactrial xs = let
                         y = sum $ map (factorials!!) xs
                         z = foldr1 (\x y -> x + y * 10) xs
                       in
                         if y == z then Just y else Nothing

-- 各桁の階乗の和が元の値となる可能性のある最大値。
-- 999..999 (p桁) の各桁の階乗の和 p * 9! が p桁未満ならば
-- p桁以上での可能性がなくなるため、(p - 1) * 9! が候補の
-- 最大値となる。
maxCandidate :: Int
maxCandidate = maxCandidateSub (factorials !! 9) 1

maxCandidateSub x y | x < y      = x - (factorials !! 9)
                     | otherwise = maxCandidateSub (x + factorials !! 9) (y * 10)

answer :: Integer
answer = foldr (\(Just x) y -> toInteger x + y) 0
         $ filter (/= Nothing)
         $ map selfDigitFactrial
         $ multiDigitListsEqOrUnder
         $ maxCandidate

main :: IO ()
main = print answer
