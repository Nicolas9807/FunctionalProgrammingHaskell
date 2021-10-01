module Lab1 where
main = print("It works!")

mymin :: Int -> Int -> Int
mymin x y | x <= y = x | otherwise = y

mymin1 ::Int -> Int -> Int
mymin1 x y = if x <= y then x else y

f :: Int -> Int -> Int
f x y = if x > 10 then x + a else x - a where a = (y + 1) * (y + 1)

f1 :: Int -> Int -> Int
f1 x y = let a = (y + 1) * (y + 1) in if x > 10 then x + a else x - a

f2 :: Int -> Int -> Int
f2 x y | x > 10 = x + a | otherwise = x - a where a = (y + 1) * (y + 1)

later :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
later (d1, m1, y1) (d2, m2, y2)
    | (y1 > y2) && (m1 > m2) && (d1 > d2) = True
    | (y1 > y2) && (m1 > m2) && (d1 == d2) = True
    | (y1 > y2) && (m1 > m2) && (d1 < d2) = True
    | (y1 > y2) && (m1 == m2) && (d1 > d2) = True
    | (y1 > y2) && (m1 == m2) && (d1 == d2) = True
    | (y1 > y2) && (m1 == m2) && (d1 < d2) = True
    | (y1 > y2) && (m1 < m2) && (d1 > d2) = True
    | (y1 > y2) && (m1 < m2) && (d1 == d2) = True
    | (y1 > y2) && (m1 < m2) && (d1 < d2) = True
    | (y1 == y2) && (m1 > m2) && (d1 > d2) = True
    | (y1 == y2) && (m1 > m2) && (d1 == d2) = True
    | (y1 == y2) && (m1 > m2) && (d1 < d2) = True
    | (y1 == y2) && (m1 == m2) && (d1 > d2) = True
    | otherwise = False

age :: (Int, Int, Int) -> (Int, Int, Int) -> Int
age (bd, bm, by) (cd, cm, cy)
    | (bm, bd) < (cm, cd) = cy - by
    | otherwise = cy - by - 1