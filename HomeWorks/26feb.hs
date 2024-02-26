import Data.Char (isAlpha)
-- Cadena que se trasnforma en una separador de palabras
-- Input: x = "Hola Mundo"
-- Output: y = ("Hola"), ("Mundo")

-- input: "Hola Mundo Como Estas" 
-- output: [("Hola"), ("Mundo"), ("Como"), ("Estas")]

split4 :: String -> [String]
split4 "" = []
split4 xs = ys : (split4 . drop 1) zs
   where (ys, zs) = span (/=',') xs

splitChars :: String -> Char -> [String]
splitChars [] _ = [""]
splitChars (x:xs) c | x == c = split xs c
               | otherwise = [x] : split xs c

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) c | x == c = "" : split xs c --tail recursion
               | otherwise = (x : head (split xs c)) : tail (split xs c)

split2 :: String -> Char -> [String]
split2 [] _ = [""]
split2 (x:xs) c | x == c = split xs c
               | otherwise = (x : head (split xs c)) : tail (split xs c)      
takeWhile _ xs = map (\x -> (x,"")) $ split2 xs ' '


trim :: String -> Char -> [String]
trim [] _ = [""]
trim (x:xs) c | x == c = split xs c
               | otherwise = (x : head (split xs c)) : tail (split xs c)                 
