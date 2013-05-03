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

-- 桁リスト表現の自然数と、その各桁をn乗した数が等しければ
-- その自然数 (通常の表現) を返し、等しくなければNothingを返す。
powerOfDigits :: (Integral a) => a -> [a] -> Maybe a
powerOfDigits n xs = let
                       y = sum $ map (^n) xs
                       z = foldr1 (\x y -> x + y * 10) xs
                     in
                       if y == z then Just y else Nothing

-- 各桁をn乗した和が元の値となる可能性のある最大値。
-- 999..999 (p桁) の各桁のn乗の和 p * 9^n が p桁未満ならば
-- p桁以上での可能性がなくなるため、(p - 1) * 9^n が候補の
-- 最大値となる。
maxCandidate :: (Integral a) => a -> a
maxCandidate n = maxCandidateSub (9^n) (9^n) 1

maxCandidateSub x y z | y < z      = y - x
                       | otherwise = maxCandidateSub x (y + x) (z * 10)



answer :: Int
answer = foldr (\(Just x) y -> x + y) 0
         $ filter (/= Nothing)
         $ map (powerOfDigits n)
         $ multiDigitListsEqOrUnder
         $ maxCandidate n
         where n = 5

main :: IO ()
main = print answer
