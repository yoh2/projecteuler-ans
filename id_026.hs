import Data.List (findIndex, maximumBy)

-- x: numerator
-- y: denominator
divCycleLength :: (Integral a) => a -> a -> Int
divCycleLength x y = divCycleLengthSub x y []

-- x: numerator
-- y: denominator
-- rs: 各桁をの除算をした時の余りリスト (逆順)
divCycleLengthSub :: (Integral a) => a -> a -> [a] -> Int
divCycleLengthSub 0 _ _  = 0
divCycleLengthSub x y rs = case findIndex (==r) rs of
                             Just n  -> n + 1
                             Nothing -> divCycleLengthSub (r * 10) y (r:rs)
                           where r = x `mod` y


answer :: Int
answer = fst $ maximumBy (\(_,x) (_,y) -> compare x y)
                         $ [(x,divCycleLength 1 x) | x <- [1 .. 999]]

main :: IO ()
main = print answer
