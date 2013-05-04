import Data.Char (ord)

triangles :: (Integral a) => [a]
triangles = scanl1 (+) [1..]

encodeWord :: String -> Int
encodeWord = sum . (map (\c -> ord c - ord 'A' + 1))

isTriangleWord :: String -> Bool
isTriangleWord word = (head $ dropWhile (< toInteger encoded) triangles)
                      == encoded
                      where
                        encoded = toInteger $ encodeWord word

tokenize :: Char -> String -> [String]
tokenize _ [] = []
tokenize c xs = let (token, ys) = span (/= c) xs
                in token : (tokenize c $ dropWhile (== c) ys)

-- 前後の一要素を削除するだけ。
-- なお、今回の解を求めるだけなら最後のreverseは不要。
unquote :: [a] -> [a]
unquote = reverse . tail . reverse . tail

answer :: IO Int
answer = do content <- readFile inputFile
            let words = map unquote $ tokenize ',' content
            return $ length $ filter isTriangleWord words
         where
           inputFile = "words.txt"

main :: IO ()
main = answer >>= print
