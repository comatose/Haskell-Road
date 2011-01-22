mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min' x (mnmInt xs)

min' :: Int -> Int -> Int
min' x y 
    | x > y = y
    | otherwise = x

mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

removeFst :: Int -> [Int] -> [Int]
removeFst _ [] = []
removeFst m (x:xs)
    | m == x = removeFst m xs
    | otherwise = x : (removeFst m xs)

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = x : (srtInts (removeFst x xs))
    where x = mnmInt xs

average' :: [Int] -> Float
average' xs = fromIntegral (sum xs) / fromIntegral (length xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

count' :: Char -> [Char] -> Int
count' _ [] = 0
count' c (x:xs) 
    | c == x = 1 + count' c xs
    | otherwise = count' c xs

prefix' :: String -> String -> Bool
prefix' [] _ = True
prefix' _ [] = False
prefix' (x:xs) (y:ys)
    | x == y = prefix' xs ys
    | otherwise = False

substrings' :: String -> String -> Bool
substrings' [] _ = True
substrings' _ [] = False
substrings' xs ys = prefix' xs ys || substrings' xs (tail ys)
