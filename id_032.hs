-- 力技。
-- せめてもうちょっと効率的な枝刈りをしたい。

pickup1 :: [a] -> [(a, [a])]
pickup1 []     = error "empty list."
pickup1 [x]    = [(x, [])]
pickup1 (x:xs) = (x, xs) : (map (\(y, ys) -> (y, x:ys)) $ pickup1 xs)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = foldr1 (++)
                  $ map (\(y, ys) -> map (y:) $ permutations ys)
                  $ pickup1 xs

split2 :: [a] -> [([a], [a])]
split2 []         = error "too short list."
split2 [_]        = error "too short list."
split2 [x0, x1]   = [([x0], [x1])]
split2 (x0:x1:xs) = ([x0], x1:xs) : (map (\(ys, zs) -> (x0:ys, zs)) $ split2 (x1:xs))

split3 :: [a] -> [([a], [a], [a])]
split3 xs = let
              pairs = filter ((>= 2) . length . snd) $ split2 xs
            in
              foldr1 (++)
              $ map (\(ys, zs)
                       -> map (\(zs1, zs2) -> (ys, zs1, zs2))
                              $ split2 zs)
                    pairs

digitListToInt :: (Integral a) => [a] -> a
digitListToInt = foldl1 (\x y -> x * 10 + y)

predicate :: (Integral a) => ([a], [a], [a]) -> Bool
predicate (xs, ys, zs) = let
                           x = digitListToInt xs
                           y = digitListToInt ys
                           z = digitListToInt zs
                         in
                           x * y == z

unique :: (Eq a) => [a] -> [a]
unique []     = []
unique (x:xs) = let ys = unique xs in
                if elem x ys then ys else x:ys
answer :: Int
answer = sum
         $ unique
         $ map (\(_, _, zs) -> digitListToInt zs)
         $ foldr1 (++)
         $ map (\xs -> filter (\(xs, ys, zs) -> (xs < ys) && predicate (xs, ys, zs))
                              $ split3 xs)
         $ permutations [1 .. 9]

main :: IO ()
main = print answer
