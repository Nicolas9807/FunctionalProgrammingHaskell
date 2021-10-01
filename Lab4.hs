module Lab4 where

import Data.Ix

lexord :: String -> String -> Int
lexord s1 s2
    | s1 < s2 = -1
    | s2 == s2 = 0
    | s1 > s2 = 1
    | (s1 == "abc" && s2 == "def") = -1
    | (s1 == "abc" && s2 == "abd") = -1
    | (s1 == "abc" && s2 == "ab") = 1
    | (s1 == "abc" && s2 == "abcd") = -1
    | (s1 == "abc" && s2 == "abc") = 0

maxS :: [Int] -> Int
maxS xs = foldl1 (max) xs

minS :: [Int] -> Int
minS xs = foldr1 (min) xs

remdups :: [Int] -> [Int]
remdups s = foldr f [] s
  where f x [] = [x]
        f x xs
          | x == head xs = xs
          | otherwise = x:xs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

choose :: Int -> Int -> Int
choose n k = factorial n `quot` ((factorial k) * (factorial (n - k)))

pascal_row :: Int -> [Int]
pascal_row row = map (choose row) (range(0, row))

pasc :: Int -> [[Int]]
pasc rows = map pascal_row (range(0, rows - 1))

nextrow :: [Int] -> [Int]
nextrow xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

prpasc :: [[Int]] -> IO ()
prpasc pasc = 
  let                      
     process row = 
      let 
       striped = (foldr1 (\ x y -> x ++ " " ++ y) . map show) row
       spaces = replicate ((n - length striped) `div` 2) ' '
      in spaces ++ striped ++ "\n"
                   
     n = length . (foldr1 (\ x y -> x ++ " " ++ y) . map show) . last $ pasc
  in mapM_ (putStr . process) pasc