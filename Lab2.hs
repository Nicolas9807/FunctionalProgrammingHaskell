module Lab2 where
main = print("It works!")

multiply :: Int -> Int -> Int
multiply x 1 = x
multiply x y = x + multiply x (y-1)

smallest :: [Int] -> Int
smallest [] = error "smallest: list must be non-empty"
smallest [x] = x
smallest (x : y : xs)
    |x > y = smallest (y : xs)
    |x < y = smallest (x : xs)
    |x == y = smallest (x : xs)

enumList :: Int -> Int -> [Int]
enumList x y
    | x > y = x : enumList (x - 1) y
    | x == y = [x]
    | otherwise = x : enumList (x + 1) y

enumList1 :: Int -> Int -> Int -> [Int]
enumList1 from to step
    | from == to = [from]
    | step == 0 = [from]
    | ((from > to && step < 0) || (from < to && step > 0)) = from : enumList1 (from + step) to step
    | ((from > to && from + step >= to) || (from < to && from + step <= to)) = []
    | otherwise = error "input data is incorrect"