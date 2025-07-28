-- (j, k) = (1, 2), (2, 3), (1, 3), (3, 4), (2, 4), (1, 4) ... といった順に探索していく方針。
-- それまでに見付かった、条件を満たす最小の D_j_k より D_{k - 1}_k が大きくなった時点で探索打ち切り。
import Data.List

p :: Int -> Int
p n = n * (3 * n - 1) `div` 2

ps :: [Int]
ps = map p [1..]

isP v = isPImpl ps where
        isPImpl (p:ps) | p < v  = isPImpl ps
                       | p == v = True
                       | p > v  = False

-- 最初に見付かった条件を満たす D_j_k と k の組を返す
firstCandidate :: (Int, Int)
firstCandidate = let Just (j, k) = find (\(j, k) -> isP (p k - p j) && isP (p k + p j))
                                        [(j, k) | k <- [2..], j <- [k - 1, k - 2 .. 1]]
                 in
                     (p k - p j, k)

solve :: Int -> Int -> Int
solve c k = if p k - p (k - 1) >= c
            then c
            else
                let
                  pk = p k
                  tuples = map (\j -> p j) [k - 1, k - 2 .. 1]
                  tuples' = takeWhile (\pj -> pk - pj < c) tuples
                in
                  case find (\pj -> isP (pk - pj) && isP (pk + pj)) tuples' of
                      Just pj -> solve (pk - pj) (k + 1)
                      Nothing -> solve c (k + 1)

main = let (d, k) = firstCandidate
       in print $ solve d (k + 1)
