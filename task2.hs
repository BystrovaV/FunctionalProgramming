import Data.List (unfoldr, foldr)

-- Развернуть натуральное число n в список всех чисел, меньших его.
less = unfoldr (\x -> if x >= 0 then Just(x, x - 1) else Nothing)

-- Развернуть число в список разрядов его двоичного представления.
toBinary = reverse . unfoldr (\x -> if x == 0 then Nothing else Just(mod x 2, div x 2))

-- Список разрядов преобразовать свёрткой в значение числа.
fromBinaryCore (s, p) d = (s + d * p, p * 2)
fromBinary ds = fst $ foldl fromBinaryCore (0, 1) $ reverse ds

-- Развернуть число в список его простых делителей.
divisorsCore (x, p) 
    | p == 0 = Nothing
    | mod x p == 0 = Just(p, (x, pred p))
    | otherwise = Just(0, (x, pred p))

divisors x = filter(/= 0) $ unfoldr divisorsCore (x, pred x)

-- Выразить список первых n чисел Фибоначчи через развёртку.

fibonachiCore (a, b, n) = 
    if n == 0 
        then Nothing 
    else Just(a, (b, a + b, pred n))

fibonachi n = unfoldr fibonachiCore (0, 1, n)

-- модификация: бесконечный список;
fibonachiInfCore (a, b) = Just(a, (b, a + b))

fibonachiInf = unfoldr fibonachiInfCore (0, 1)

-- Развернуть число в сиракузскую последовательность.
collatzCore x 
    | x == 0 = Nothing
    | x == 1 = Just (x, 0)
    | even x = Just (x, div x 2)
    | otherwise = Just (x, 3 * x + 1)
    

collatz = unfoldr collatzCore

-- Выразить список простых чисел, не превышающих n, через развёртку
-- с помощью решета Эратосфена;
primeCore ([], _) = Nothing
primeCore ((x:xs), n) = 
    if x > n 
        then Nothing
    else Just(x, (filter (\n -> mod n x /= 0) xs, n))

prime n = unfoldr primeCore $ ((2:) $ [3, 5 ..], n)

-- модификация: бесконечный список всех простых чисел;
primeInfCore [] = Nothing
primeInfCore (x:xs) = Just(x, filter (\n -> mod n x /= 0) xs)

primeInf = unfoldr primeInfCore $ (2:) $ [3, 5 ..]


-- тип “двоичное дерево” 
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

tree :: BinaryTree Int
tree = Node 1 (Node 2 (Node 3 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 5 EmptyTree EmptyTree)

treeS :: BinaryTree String
treeS = Node "l" (Node "f" (Node "e" EmptyTree EmptyTree) (Node "k" EmptyTree EmptyTree)) (Node "d" EmptyTree EmptyTree)

-- Поэлементное преобразование, сохраняющее структуру (аналог map);
mapTree :: (a -> a) -> BinaryTree a -> BinaryTree a
mapTree _ EmptyTree = EmptyTree
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

-- Подсчёт элементов
countElements :: BinaryTree a -> Int

countElements EmptyTree = 0
countElements (Node value left right) = 1 + countElements left + countElements right


-- результат обхода накапливать с помощью функции f типа a -> b -> b,
-- где a - тип элемента дерева, b - тип результата обхода;
-- функция treeTraverseD принимает: функцию f, начальное значение b и дерево.

-- Здесь я поменяла аргументы местами, т.к. например конкатенация присоединяет b к a
treeTraverseD :: (b -> a -> b) -> b -> BinaryTree a -> b

treeTraverseD _ b EmptyTree = b
treeTraverseD f b (Node value left right) = treeTraverseD f (f (treeTraverseD f b left) value) right


treeTraverseW :: (b -> a -> b) -> b -> BinaryTree a -> b

treeTraverseW _ acc EmptyTree = acc

treeTraverseW f acc tree = traverse [tree] acc
  where
    traverse [] acc = acc
    traverse (EmptyTree:xs) acc = traverse xs acc
    traverse (Node value left right:xs) acc =
      let newAcc = f acc value
          newQueue = xs ++ [left, right]
      in traverse newQueue newAcc


-- Алгебраические выражения
-- листья суть переменные и константы;
-- унарной операции соответствует внутренний узел с одним поддеревом;
-- бинарной операции соответствует внутренний узел с двумя поддеревьями.
-- data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)

data Expr a
  = Constant a                    -- Константа
  | Variable String a           -- Переменная
  | UnaryOp (a -> a) String (Expr a)      -- Унарная операция
  | BinaryOp (a -> a -> a) String (Expr a) (Expr a)  -- Бинарная операция

expr :: Expr Double
expr = BinaryOp (*) "*" (BinaryOp (+) "+" (UnaryOp cos "cos" (Variable "x" 5)) (UnaryOp sqrt "sqrt" (Constant 2.0))) (Constant 8)

exprToString :: Show a => Expr a -> String
exprToString (Constant val) = show val
exprToString (Variable name _) = name
exprToString (UnaryOp _ name expr) = name ++ "(" ++ exprToString expr ++ ")"
exprToString (BinaryOp _ name expr1 expr2) = "(" ++ exprToString expr1 ++ " " ++ name ++ " " ++ exprToString expr2 ++ ")"

exprCalculate (Constant val) = val
exprCalculate (Variable _ val) = val
exprCalculate (UnaryOp op _ expr) = op $ exprCalculate expr
exprCalculate (BinaryOp op _ expr1 expr2) = op (exprCalculate expr1) (exprCalculate expr2)


expr2 :: Expr Double
expr2 = BinaryOp (*) "*" (BinaryOp (+) "+" (BinaryOp (+) "+" (Constant 0) (Constant 0)) (Constant 5)) (Constant 5)

exprSimplifyHelper op name expr1 expr2 = 
  case (name, expr1, expr2) of
    ("-", Constant 0, _) -> expr2
    ("-", _, Constant 0) -> expr1
    ("+", Constant 0, _) -> expr2
    ("+", _, Constant 0) -> expr1
    ("*", Constant 0, _) -> Constant 0
    ("*", _, Constant 0) -> Constant 0
    ("*", Constant 1, _) -> expr2
    ("*", _, Constant 1) -> expr1
    _ -> BinaryOp op name expr1 expr2

exprSimplify (Constant val) = Constant val
exprSimplify (Variable name val) = Variable name val
exprSimplify (UnaryOp op name expr) = UnaryOp op name expr

exprSimplify (BinaryOp op name expr1 expr2) = 
  let simplifiedExpr1 = exprSimplify expr1
      simplifiedExpr2 = exprSimplify expr2
  in exprSimplifyHelper op name simplifiedExpr1 simplifiedExpr2

-- expr1 :: Expr Double
expr1 = UnaryOp sin "sin" (BinaryOp (-) "-" (BinaryOp (*) "*" (Constant 3) (Variable "x" 5)) (Constant 2))

derivate (Constant val) _ = Constant 0
derivate (Variable name val) arg 
    | arg == name = Constant 1
    | otherwise = Constant 0

derivate (BinaryOp op "-" expr1 expr2) arg = BinaryOp op "-" (derivate expr1 arg) (derivate expr2 arg)
derivate (BinaryOp op "+" expr1 expr2) arg = BinaryOp op "+" (derivate expr1 arg) (derivate expr2 arg)

derivate (BinaryOp op "*" expr1 expr2) arg = 
  BinaryOp op "+" (BinaryOp op "*" (derivate expr1 arg) expr2) (BinaryOp op "*" expr1 (derivate expr2 arg))

derivate (BinaryOp op "/" expr1 expr2) arg = 
  BinaryOp op "/" (
    BinaryOp op "-" (BinaryOp op "*" (derivate expr1 arg) expr2) (BinaryOp op "*" expr1 (derivate expr2 arg))
  ) 
  (BinaryOp op "*" expr2 expr2)

derivate (UnaryOp op "sin" expr) arg = BinaryOp (*) "*" (UnaryOp op "cos" expr) (derivate expr arg)
derivate (UnaryOp op "cos" expr) arg = BinaryOp (*) "*" (BinaryOp (*) "*" (Constant -1) (UnaryOp op "sin" expr)) (derivate expr arg)
