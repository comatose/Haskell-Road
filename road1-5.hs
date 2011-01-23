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

blowup :: String -> String
blowup = blowup' 1

blowup' _ [] = []
blowup' n (x:xs) = (replicate' n x) ++ blowup' (n + 1) xs 

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n c = c : replicate' (n - 1) c

factors :: Integer -> [Integer]
factors 1 = []
factors n = p : factors n'
    where
        p = ld n
        n' = n `div` p

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' _ [] = []

lengths :: [[a]] -> [Int]
lengths = map length

filter' :: (a -> Bool) -> [a] -> [a]
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs
filter' _ [] = []

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf i n
    | i^2 > n = n
    | n `rem` i == 0 = i
    | otherwise = ldf (i + 1) n

prime0 :: Integer -> Bool
prime0 n = ld n == n

primes0 :: [Integer]
primes0 = 2 : filter prime0 [3..]

ldp :: Integer -> Integer
ldp = ldpf primes1

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n
    | rem n p == 0 = p
    | p^2 > n = n
    | otherwise = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n
    | n < 1 = error "not a positive integer"
    | n == 1 = False
    | otherwise = ldp n == n
