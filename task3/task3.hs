import Data.Function (fix)

-- -- наибольший общий делитель
gcdHelper f a b = if a == b then a
                  else if a > b then f (a - b) b
                  else f a (b - a)

gcdFix :: Int -> Int -> Int
gcdFix = fix gcdHelper

-- бесконечный список чисел Фибоначчи;
infFinHelper f a b = a : (f b (a + b))
infFibFix = fix infFinHelper 0 1

-- вычисление числа Фибоначчи по номеру;
fibHelper f a _ 1 = a
fibHelper f a b n = f b (a + b) (pred n)
fibFix :: Int -> Int
fibFix n = fix fibHelper 0 1 n

-- вычисление суммы списка;
sumHelper f [] s = s
sumHelper f (x:xs) s = f xs (s + x)

sumFix :: [Int] -> Int
sumFix x = fix sumHelper x 0

-- нахождение наименьшего элемента в списке;
minHelper f [] m = m
minHelper f (x:xs) m
    | x < m = f xs x
    | otherwise = f xs m

minFix :: [Int] -> Int
minFix x = fix minHelper x (maxBound :: Int)

-- реверс списка;
reverseHelper f [] ys = ys
reverseHelper f (x:xs) ys = f xs (x:ys)

reverseFix :: [a] -> [a]
reverseFix x = fix reverseHelper x []

