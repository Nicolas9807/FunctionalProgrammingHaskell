module Lab3 where

import observe

multiply :: Int -> Int -> Int
multiply 0 b = 0
multiply 1 b = b
multiply a b = b + multiply (a - 1) b
multiply a b = b + multiply (observe "a" a - 1) b
