import Data.List
import Data.Char

alphaValue :: String -> Int
alphaValue = sum . (map (\c -> ord c - ord 'A' + 1))

tokenize :: Char -> String -> [String]
tokenize _ [] = []
tokenize c xs = let (token, ys) = span (/= c) xs
                in token : (tokenize c $ dropWhile (== c) ys)

-- 前後の一要素を削除するだけ。
-- なお、今回の解を求めるだけなら最後のreverseは不要。
unquote :: [a] -> [a]
unquote = reverse . tail . reverse . tail

answer :: IO Integer
answer = do content <- readFile "names.txt"                 -- 読み込んで
            let names = map unquote $ tokenize ',' content  -- 名前のリストを得る。
            -- [1..]とソートした名前のリストを対応付けることでスコア計算。その和をとる。
            return $ sum $ zipWith (\x y -> x * (toInteger $ alphaValue y))
                                   [1..] $ sort names

main :: IO ()
main = answer >>= print
