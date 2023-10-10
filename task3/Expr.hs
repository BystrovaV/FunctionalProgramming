module Expr where
import Data.Map (Map, (!), fromList)

data Expr unop binop a
    = UnOpNode unop (Expr unop binop a)
    | BinOpNode binop (Expr unop binop a) (Expr unop binop a)
    | Const a
    | Var String

type UnDecoder unop a = unop -> (a -> a)
type BinDecoder binop a = binop -> (a -> a -> a)
type Binding a = String -> a

eval :: UnDecoder unop a -> BinDecoder binop a -> Binding a -> Expr unop binop a -> a
eval _ _ _ (Const x) = x
eval _ _ binding (Var s) = binding s
eval decodeUn decodeBin binding (BinOpNode op e1 e2) = let
    v1 = eval decodeUn decodeBin binding e1
    v2 = eval decodeUn decodeBin binding e2
    f  = decodeBin op
  in f v1 v2
eval decodeUn decodeBin binding (UnOpNode op e) = let
    v  = eval decodeUn decodeBin binding e
    f  = decodeUn op
  in f v

data FloatingUnOp
    = Neg
    | Sin
    | Cos
    deriving (Show, Eq, Ord)

data FloatingBinOp
    = Add
    | Sub
    | Mul
    | Div
    deriving (Show, Eq, Ord)

floatingUnSemantic = fromList [
    (Neg,  negate),
    (Sin,  sin),
    (Cos,  cos)
  ]

floatingBinSemantic = fromList [
    (Add,  (+)),
    (Sub,  (-)),
    (Mul,  (*)),
    (Div,  (/))
  ]

type FloatingExpr = Expr FloatingUnOp FloatingBinOp Double

evalFloat varMap = eval (floatingUnSemantic !) (floatingBinSemantic !) (varMap !)

