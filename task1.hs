-- наибольший общий делитель
myGcd a b
  | a == b = a
  | a > b = myGcd (a - b) b
  | a < b = myGcd a (b - a)

-- возведение в степень
powerOptimised x 1 = x
powerOptimised x k =
  if even k
    then powerOptimised (x * x) (div k 2)
    else x * powerOptimised x (pred k)

-- Фибоначчи оптимизированный
matrixMult [a1, b1, c1, d1] [a2, b2, c2, d2] =
  [a1 * a2 + b1 * c2, a1 * b2 + b1 * d2, c1 * a2 + d1 * c2, c1 * b2 + d1 * d2]

fibPower [a, b, c, d] 1 = [a, b, c, d]
fibPower [a, b, c, d] n =
  if even n
    then fibPower (matrixMult [a, b, c, d] [a, b, c, d]) (div n 2)
    else matrixMult [a, b, c, d] (fibPower [a, b, c, d] (pred n))

fibOptimised n = head $ tail $ fibPower [0, 1, 1, 1] n

-- countDivisorsUpTo x 1 = 1
-- countDivisorsUpTo x p = 
--     let m = if mod x p == 0 then 1 else 0
--     in m + countDivisorsUpTo x (pred p)

-- совершенные числа
sumDivisors _ 1 = 1
sumDivisors x p =
  let m = if mod x p == 0 then p else 0
  in m + sumDivisors x (pred p)

isPerfect x = sumDivisors x (pred x) == x

-- длина сиракузской последовательности
collatz n
  | n == 1 = 1
  | even n = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3 * n + 1)

-- числа деланнуа
delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy m n = delannoy (pred m) n + delannoy m (pred n) + delannoy (pred m) (pred n)

-- вычисление многочлена
xReverseHelper [] ys = ys
xReverseHelper (x:xs) ys = xReverseHelper xs (x:ys)

xReverse xs = xReverseHelper xs []

evalPolynomialHelper [] x = 0
evalPolynomialHelper (p:ps) x = p * x + evalPolynomialHelper ps x * x

evalPolynomial p x =
  let (r: rs) = xReverse p
  in r + evalPolynomialHelper rs x


-- клонирование элементов
cloneOne 1 x ys = ys ++ [x]
cloneOne n x ys = cloneOne (pred n) x (ys ++ [x])

cloneHelper _ [] ys = ys
cloneHelper n (x:xs) ys = cloneHelper n xs (cloneOne n x ys)

clone x n = cloneHelper n x []

-- сшивание списков бинарной операцией
xZipWithHelper _ [] _ zs = zs
xZipWithHelper _ _ [] zs = zs
xZipWithHelper f (x:xs) (y:ys) zs = xZipWithHelper f xs ys (zs ++ [f x y])

xZipWith f x y = xZipWithHelper f x y []

-- первые числа фибоначчи
infiniteFib = 0 : 1 : zipWith (+) infiniteFib (tail infiniteFib)

generalizedFibonacciHelper x = sum x : generalizedFibonacciHelper (tail x ++ [sum x])
generalizedFibonacci x = x ++ generalizedFibonacciHelper x

fromDigitsHelper [] _ = 0
fromDigitsHelper (x:xs) n = x * n + fromDigitsHelper xs n * n

fromDigits n x =
  let (r: rs) = xReverse x
  in r + fromDigitsHelper rs n

-- to digits
toDigitsHelper _ 0 r = r
toDigitsHelper n x r =
  toDigitsHelper n (div x n) (mod x n :r)

toDigits n x = toDigitsHelper n x []

-- сложение чисел
addDigitWiseHelper n [] [] r 0 = r
addDigitWiseHelper n [] [] r k = k : r

addDigitWiseHelper n [] (y:ys) r k = addDigitWiseHelper n [] ys (mod (y + k) n : r) (div (y + k) n)
addDigitWiseHelper n (x:xs) [] r k = addDigitWiseHelper n xs [] (mod (x + k) n : r) (div (x + k) n)
addDigitWiseHelper n (x:xs) (y:ys) r k = addDigitWiseHelper n xs ys (mod (x + y + k) n : r) (div (x + y + k) n)

addDigitWise n x y = 
  addDigitWiseHelper n (reverse x) (reverse y) [] 0

delannoyPath 0 0 = [[]]
delannoyPath 0 b = map (2:) (delannoyPath 0 (pred b))
delannoyPath a 0 = map (0:) (delannoyPath (pred a) 0)
delannoyPath a b = map (0:) (delannoyPath (pred a) b) ++ map (2:) (delannoyPath a (pred b)) ++ map (1:) (delannoyPath (pred a) (pred b))