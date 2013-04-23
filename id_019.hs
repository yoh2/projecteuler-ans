-- Weekday definitions
data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

instance Eq Weekday where
  Sunday    == Sunday    = True
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  _         == _         = False

  x /= y = not (x == y)

instance Show Weekday where
  show Sunday    = "Sunday"
  show Monday    = "Monday"
  show Tuesday   = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday  = "Thursday"
  show Friday    = "Friday"
  show Saturday  = "Saturday"


-- 曜日計算を楽に行うために、数値との相互変換を定義。
intWeekdayPair :: (Integral a) => [(a, Weekday)]
intWeekdayPair = [(0, Sunday), (1, Monday), (2, Tuesday), (3, Wednesday),
                  (4, Thursday), (5, Friday), (6, Saturday)]

intToWeekday :: (Integral a) => a -> Weekday
intToWeekday n = w where (Just w) = lookup (n `mod` 7) intWeekdayPair

weekdayToInt :: (Integral a) => Weekday -> a
weekdayToInt w = n where (Just n) = lookup w $ map (\(x, y) -> (y, x)) intWeekdayPair


weekdayAfter :: (Integral a) => Weekday -> a -> Weekday
weekdayAfter w n = intToWeekday $ (`mod` 7) $ weekdayToInt w + n

-- months, years
monthdays, leapYearMonthdays :: (Num a) => [a]
monthdays         = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapYearMonthdays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

isLeapYear :: (Integral a) => a -> Bool
isLeapYear y = (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0))

monthdaysOfYear :: (Integral a, Num b) => a -> [b]
monthdaysOfYear y = if isLeapYear y then leapYearMonthdays else monthdays

-- 20世紀最初 (1901-01-01) の曜日 = 1900-01-01 (月) から365日後
weekdayOf20thHead :: Weekday
weekdayOf20thHead = weekdayAfter Monday $ sum $ monthdaysOfYear 1900

-- takeしているのは、最後の要素 (2001-01-01に対応) を削るため。
monthHeadWeekdays20th :: [Weekday]
monthHeadWeekdays20th = take ((2000 - 1901 + 1) * 12) $ scanl weekdayAfter weekdayOf20thHead
                        $ foldr1 (++) $ map monthdaysOfYear [1901 .. 2000]

answer :: Int
answer = length $ filter (== Sunday) monthHeadWeekdays20th

main :: IO ()
main = print answer
