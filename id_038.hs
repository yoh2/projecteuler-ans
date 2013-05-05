import Data.List (permutations)
import Data.Maybe (fromJust, isJust)


-- 指定した個数を選ぶ組合せのリストを得る。
combinations :: (Num a, Ord a) => a -> [b] -> [[b]]
combinations 0 _                  = [[]]
combinations _ []                 = []
combinations n (x:xs) | n < 0     = []
                      | otherwise = map (x:) (combinations (n - 1) xs) ++ combinations n xs


addDigitList :: Integral a => [a] -> [a] -> [a]
addDigitList xs ys = reverse $ addReversedDigitList (reverse xs) (reverse ys)

addReversedDigitList :: Integral a => [a] -> [a] -> [a]
addReversedDigitList xs ys = addReversedDigitListWithCarry xs ys 0
                             where
                               addReversedDigitListWithCarry []     []     0 = []
                               addReversedDigitListWithCarry []     []     1 = [1]
                               addReversedDigitListWithCarry []     (9:ys) 1 = 0 : addReversedDigitListWithCarry [] ys 1
                               addReversedDigitListWithCarry []     (y:ys) c = (y + c) : ys
                               addReversedDigitListWithCarry (9:xs) []     1 = 0 : addReversedDigitListWithCarry xs [] 1
                               addReversedDigitListWithCarry (x:xs) []     c = (x + c) : xs
                               addReversedDigitListWithCarry (x:xs) (y:ys) c
                                 = let s = x + y + c in
                                   if s >= 10 then (s - 10) : addReversedDigitListWithCarry xs ys 1
                                   else             s       : addReversedDigitListWithCarry xs ys 0

-- リストに重複があるか否か
hasSameValue :: Eq a => [a] -> Bool
hasSameValue [] = False
hasSameValue (x:xs) = x `elem` xs || hasSameValue xs

-- pandigital として不正な値か否か。
-- 重複している桁があるか、0が含まれていれば不正。
badPandigital :: Integral a => [a] -> Bool
badPandigital xs = 0 `elem` xs || hasSameValue xs

-- 指定した自然数 (桁リスト表現) から pandigital multiple を
-- 得られるならばそれを結合したリスト。
-- 得られなければ Nothing。
concatnatedPandigitalMultiples :: Integral a => [a] -> Maybe [a]
concatnatedPandigitalMultiples xs
    = helper xs xs xs
      where
        helper delta sum digits | length digits == 9 = Just digits
                                | otherwise          = if badPandigital next || (or $ map (`elem` digits) next) then Nothing
                                                       else helper delta next (digits ++ next)
                                                       where
                                                         next = addDigitList delta sum

-- ひとつめの整数 (x 1された数) について全パターンを試し、
-- pandigital multiplesを構成できるものから最大値を選ぶ。
-- n > 1なので、最低限2つの整数からなり、また、ふたつめ以降の
-- 整数の桁数は明らかにひとつめの整数の桁数以上なので、
-- ひとつめの整数の桁数は最大でも 9 div 2 = 4 桁。
-- 1～4桁の整数の全パターンはわずか3609通りなので力技で十分解決できる。
answer :: Integer
answer = foldl1 (\x y -> x * 10 + y)
         $ maximum
         $ map fromJust
         $ filter isJust
         $ map concatnatedPandigitalMultiples
         $ concat $ map permutations
         $ concat $ map ((flip combinations) [1..9]) [1 .. 4]


main :: IO ()
main = print answer
