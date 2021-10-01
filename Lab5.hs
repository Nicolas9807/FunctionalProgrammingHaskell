module Main (main) where

data Month = Jan | Feb | Mar | Apr | May | June | July | Aug | Sept| Oct | Nov | Dec

season :: Month -> String
season Jan = "Winter"
season Feb = "Winter"
season Mar = "Spring"
season Apr = "Spring"
season May = "Spring"
season June = "Summer"
season July = "Summer"
season Aug = "Summer"
season Sept = "Autumn"
season Oct = "Autumn"
season Nov = "Autumn"
season Dec = "Winter"

daysInMonth :: Int -> Month -> Int
daysInMonth year Jan = 31
daysInMonth year Feb
    | (year `mod` 400 == 0) = 29
    | ((year `mod` 400 > 0) && (year `mod` 100 >0) && (year `mod` 4 == 0)) = 29
    | otherwise = 28
daysInMonth year Mar = 31
daysInMonth year Apr = 30
daysInMonth year May = 31
daysInMonth year June = 30
daysInMonth year July = 31
daysInMonth year Aug = 31
daysInMonth year Sept = 30
daysInMonth year Oct = 31
daysInMonth year Nov = 30
daysInMonth year Dec = 31

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Ord)

maxTree :: (Ord a) => Tree a -> a
maxTree Empty = error "Tree is empty"
maxTree (Node root left right) = max root (max y z)
    where 
        y = if(left == Empty) then root else maxTree left
        z = if(right == Empty) then root else maxTree right

minTree :: (Ord a) => Tree a -> a
minTree Empty = error "Tree is empty"
minTree (Node root left right) = min root (min y z)
    where 
        y = if(left == Empty) then root else minTree left
        z = if(right ==Empty) then root else minTree right

flatten :: Tree a -> [a]   
flatten Empty = []         
flatten (Node root left right) = flatten left ++ flatten right ++ [root]

sInsert :: Ord a => a -> Tree a -> Tree a
sInsert a Empty = Node a Empty Empty
sInsert a (Node root left right)
    | a == root = Node root left right
    | a > root = Node a left (sInsert root right)
    | a < root = Node a (sInsert root left) right

tsort :: Ord a => [a] -> [a]
tsort [] = []
tsort [x] = [x]
tsort xs = merge (tsort ys) (tsort zs) where (ys,zs) = split xs

split :: [a] -> ([a],[a])
split xs = (take len xs, drop len xs) where len = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x<=y then x : merge xs (y:ys) else y : merge (x:xs) ys

telem :: (Eq a) => a -> Tree a -> Bool
telem a Empty = False
telem a (Node root left right)
    | a == root = True
    | otherwise = telem a left || telem a right

showT :: Show a => Tree a -> Int -> String
showT (Empty) _ = []
showT (Node root left right) a = replicate a '*' ++ show root ++ "\n" ++ showT left (a+1) ++ showT right (a+1)

bal :: (Ord a) => [a] -> Tree a
bal xs = foldl (\acc x -> sInsert x acc) Empty xs

main :: IO ()
main = do
    let a = Node "Parent" (Node "childLeft" (Node "grandChildLeftLeft" Empty Empty) Empty) (Node "childRight" Empty Empty)
    let d = bal [11,8,21,3,1,45]
    
    putStrLn (showT a 0)
    putStrLn (showT d 0)

    print (bal [1..10])
    
    print (telem 1 d)

    print (flatten d)

    print (telem 22 d)
    let z = flatten d
    let y = 10
    print (tsort z)
    print(d)
    print (flatten d)
    let tree1 = sInsert 100 d
    print (tree1)

    putStr("Minimum element of tree: ")
    print(minTree tree1)
    putStr("Maximum element of tree: ")
    print(maxTree tree1)