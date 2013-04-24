-- import Data.Listして、
--   (sort $ permutations ['0' .. '9']) !! 999999
-- で解が求まるが、ちょっとやってみたくなったので組み合わせ列挙を自作。

-- リストからひとつ取り出して、その値と残り要素リストのペアのリスト
-- を作成して返す。
-- 取り出す値は、要素の先頭から順に取り出される。つまり、
--   [(先頭の値, 残り要素リスト),
--    (2番目の値, 残り(1番目要素も含む)要素リスト),
--    (3番目の値, 残り(1,2番目要素も含む)要素リスト),
--    ...
--    (最後の値, 残り(最初から最後直前まで)要素リスト)
--   ]
-- というリストが返される。
-- 元のリストには最低ひとつの要素が必要。
pickupList :: [a] -> [(a, [a])]
pickupList []     = error "empty list."
pickupList [x]    = [(x, [])]
pickupList (x:xs) = (x, xs) : (map (\(y, ys) -> (y, x:ys)) $ pickupList xs)

-- ordered list -> ordered permuations
-- リストの要素に重複がある場合は未対応 (結果も重複する)
orderedPermuation :: [a] -> [[a]]
orderedPermuation [] = [[]]
orderedPermuation xs = concat $ map (\(y, ys) -> map (y :) $ orderedPermuation ys) $ pickupList xs

answer :: String
answer = orderedPermuation ['0' .. '9'] !! 999999

main :: IO ()
main = putStrLn answer
