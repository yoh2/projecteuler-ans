import Data.Map

nextCollatz :: (Integral a) => a -> a
nextCollatz n | n `mod` 2 == 0 = n `div` 2
              | otherwise      = n * 3 + 1

-- 自然数x -> 長さのマップ -> (xに対する長さ, 更新されたマップ)
recordCollatzLength :: (Integral a) => a -> Map a a -> (a, Map a a)
recordCollatzLength 1 m = (1, insert 1 1 m)
recordCollatzLength x m =
  case Data.Map.lookup x m of
    Just y  -> (y, m)
    Nothing -> let (y, m2) = recordCollatzLength (nextCollatz x) m
               in (y + 1, insert x (y + 1) m2)

-- 値が最大の要素を求める。同値がある場合はどれかひとつが適当に決まる。
maxValueWithKey :: (Ord b) => Map a b -> (a, b)
maxValueWithKey m = foldr1 (\(xk, xv) (yk, yv) -> if xv > yv then (xk, xv) else (yk, yv)) $ toList m

solution :: Integer
solution = let m = Prelude.foldr (\x m -> snd $ recordCollatzLength x m)
                                 (Data.Map.fromList []) [1 .. 1000000 - 1]
           in fst $ maxValueWithKey $ filterWithKey (\k _ -> k < 1000000) m

main :: IO ()
main = print solution
