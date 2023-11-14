--  Недавно вы делали деревья выражений — например, (BinOp Add (BinOp Mul (Const 2) (Var "x")) (Const 10)) для выражения 2*x + 10. 
--  На основе экзистенциальных типов прошу преобразовать в _типизированные_ выражения.
-- Выражения бывают двух типов: Int и Bool.  Операции +, -, *, / 
-- применяются к операндам Int и дают в результате выражение типа Int. 
--  Операции &, | применяются к операндам Bool и дают в результате выражение типа Bool.
--   Операции =, <, >, # (не равно) применяются к операндам Int
--    и дают в результате выражение типа Bool.  
--    Наконец, операция ? имеет три операнда: первый типа Bool, 
--    а два оставшихся имеют любой одинаковый тип, семантика очевидна.

class Expr a where
    toString :: a -> String
    eval :: a -> a

data IntExpr 
    = IntConst Int 
    | IntBinExpr BinOpInt IntExpr IntExpr
    | IntTernExpr BoolExpr IntExpr IntExpr
    deriving (Show)

data BoolExpr
    = BoolConst Bool
    | BoolBinExpr BinOpBool BoolExpr BoolExpr
    | CmpBinExpr BinOpCmp IntExpr IntExpr
    | BoolTernExpr BoolExpr BoolExpr BoolExpr
    deriving (Show)

data BinOpInt
    = Add 
    | Sub 
    | Mul 
    | Div 
    deriving (Show, Eq, Ord)

data BinOpBool
    = And
    | Or
    deriving (Show, Eq, Ord)

data BinOpCmp
    = Eq
    | Lt
    | Gt
    | Neq
    deriving (Show, Eq, Ord)

instance Show BinOpInt where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show BinOpBool where
  show And = "&" 
  show Or = "|"

instance Show BinOpCmp where
  show Eq = "=" 
  show Lt = "<" 
  show Gt = ">" 
  show Neq = "#" 

getValueInt (IntConst x) = x

getValueBool (BoolConst x) = x

instance Expr IntExpr where
    toString (IntConst x) = show x
    toString (IntBinExpr op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"
    toString (IntTernExpr x y z) = (toString x) ++ " ? " ++ (toString y) ++ " : " ++ (toString z)
    eval (IntConst x) = IntConst x
    eval (IntBinExpr op x y) = IntConst (applyBinOpInt op (getValueInt $ (eval x)) (getValueInt $ (eval y)))
    eval (IntTernExpr x y z) = if (getValueBool $ eval x) then eval y else eval z

instance Expr BoolExpr where
    toString (BoolConst x) = show x
    toString (BoolBinExpr op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"
    toString (CmpBinExpr op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"
    toString (BoolTernExpr x y z) = (toString x) ++ " ? " ++ (toString y) ++ " : " ++ (toString z)
    eval (BoolConst x) = BoolConst x
    eval (BoolBinExpr op x y) = BoolConst (applyBinOpBool op (getValueBool $ (eval x)) (getValueBool $ (eval y)))
    eval (CmpBinExpr op x y) = BoolConst (applyBinOpCmp op (getValueInt $ (eval x)) (getValueInt $ (eval y)))
    eval (BoolTernExpr x y z) = if (getValueBool $ eval x) then eval y else eval z

applyBinOpInt :: BinOpInt -> (Int -> Int -> Int)
applyBinOpInt Add = (+)
applyBinOpInt Sub = (-)
applyBinOpInt Mul = (*)
applyBinOpInt Div = div

applyBinOpBool :: BinOpBool -> (Bool -> Bool -> Bool)
applyBinOpBool And = (&&)
applyBinOpBool Or = (||)

applyBinOpCmp :: BinOpCmp -> (Int -> Int -> Bool)
applyBinOpCmp Eq = (==)
applyBinOpCmp Lt = (<)
applyBinOpCmp Gt = (>)
applyBinOpCmp Neq = (/=)

data SomeExpr = forall a. Expr a => SomeExpr a

showSomeExpr ::  SomeExpr -> String
showSomeExpr (SomeExpr e) = toString e

evalSomeExpr ::  SomeExpr -> SomeExpr
evalSomeExpr (SomeExpr e) = SomeExpr (eval e)

expr1 = IntBinExpr Add (IntConst 1) (IntConst 2)
expr2 = BoolBinExpr Or (BoolConst True) (BoolConst False)
expr3 = CmpBinExpr Gt (IntConst 3) (IntConst 4)
