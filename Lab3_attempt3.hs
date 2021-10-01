import Data.Function(on)

count :: String -> Char -> Int
count str c = length $ filter (== c) str

cpy :: String -> Int -> String
cpy string n = concat $ replicate n string

spaceIt :: String -> String
spaceIt [] = []
spaceIt [x] = [x]
spaceIt (x : xs) = x : ' ' : spaceIt xs


countS :: [Char] -> String -> Int
countS chars string = sum [occuranceCount c| c <- string] where occuranceCount char | elem char chars = 1 | otherwise = 0

bin :: Int -> [Int]
bin 0 = []
bin n | n > 0 = bin (n `div` 2) ++ [n `mod` 2]

add' :: Int -> [Int] -> [Int] -> [Int]
add' 0 [] [] = []
add' n [] [] = [n]
add' n xs [] = add' n xs [0]
add' n [] xs = add' n [0] xs
add' n (x:xs) (y:ys) = r : add' q xs ys
    where (q,r) = quotRem (x+y) 10

add :: [Int] -> [Int] -> [Int]
add = add' 0

addStr :: [Int] -> [Int] -> [Int]
addStr = add

proper :: Int -> [Int]
proper y = [ x | x <- [2..y-1], y `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect y
    | ((sum (proper y)) == y-1) = True
    | otherwise = False

getPerfect :: Int -> [Int]
getPerfect y = [x | x <- [2 .. y-1], (isPerfect x) == True]
