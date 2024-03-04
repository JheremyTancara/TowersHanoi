module Class2902 (tuplaNameAndAge, sumAges, averageAge, names, underAge, betweenAge, convertToInt) where
import Data.Char (isDigit)
import Data.List

convertToInt :: String -> Int
convertToInt str = read str :: Int

tuplaNameAndAge :: String -> Char -> [(String, Int)]
tuplaNameAndAge [] _ = []
tuplaNameAndAge (x:xs) c  | x == c = tuplaNameAndAge (tail xs) c
                | otherwise = (x : init (takeWhile (not . isDigit) xs), convertToInt (takeWhile isDigit (dropWhile (not . isDigit) xs)) :: Int) : tuplaNameAndAge (dropWhile (/=c) xs) c

sumAges :: [(String, Int)] -> Int
sumAges [] = 0
sumAges [x] = snd x
sumAges (x:xs) = snd x + sumAges xs

averageAge :: [(String, Int)] -> Int
averageAge [] = 0
averageAge x = div (sumAges x) (length x)

names :: [(String, Int)] -> [String]
names [] = []
names [x] = [fst x]
names (x:xs) = fst x : names xs

underAge :: [(String, Int)] -> Int -> [String]
underAge [] _ = []
underAge _ 0 = []
underAge [x] c  | snd x < c = [fst x]
                | otherwise = []
underAge (x:xs) c = underAge [x] c ++ underAge xs c


betweenAge :: [(String, Int)] -> Int -> Int -> [(String, Int)]
betweenAge [] _ _ = []
betweenAge _ 0 _ = []
betweenAge _ _ 0 = []
betweenAge [a] x y  | snd a >= x && snd a <= y = [a]
                    | otherwise = []
betweenAge (a:as) x y = betweenAge [a] x y ++ betweenAge as x y


