import Data.List (foldl)
import qualified Data.Map as Map
import Expr


data Command
  = Push Double        
  | PushVar String
  | Pop             
  | Add             
  | Subtract        
  | Multiply        
  | Divide          
  deriving Show


data MachineState = MachineState
  { stack :: [Double]      
  , registers :: Map.Map String Double
  } deriving Show

executeCommand :: Command -> MachineState -> MachineState
executeCommand (Push x) state = state { stack = x : stack state }
executeCommand Pop state = state { stack = tail (stack state) }

executeCommand Main.Add state = state { stack = (x + y) : rest }
  where (y : x : rest) = stack state

executeCommand Subtract state = state { stack = (x - y) : rest }
  where (y : x : rest) = stack state

executeCommand Multiply state = state { stack = (x * y) : rest }
  where (y : x : rest) = stack state

executeCommand Divide state
    | y /= 0 = state { stack = (x / y) : rest }
    | otherwise = error "Division by zero"
    where
      (y : x : rest) = stack state

executeCommand (PushVar name) state = state { stack = (case Map.lookup name (registers state) of
    Just value -> value
    Nothing -> error ("Variable " ++ name ++ " is undefined"))
     : stack state }


executeCommandsCore state cmd = (executeCommand cmd state)
executeCommands commands initialState = foldl executeCommandsCore initialState commands

initialState = MachineState { stack = [], registers = Map.fromList[("x", 5)] }
commands = [Push 5, Pop, Push 4]

parseCommands input = parseCommand (words input) []

parseCommand [] ys = ys
parseCommand ("PushVar":x:xs) ys = parseCommand xs (ys ++ [PushVar x])
parseCommand ("Push":x:xs) ys = parseCommand xs (ys ++ [Push (read x)])
parseCommand ("Pop":xs) ys = parseCommand xs (ys ++ [Pop])
parseCommand ("Add":xs) ys = parseCommand xs (ys ++ [Main.Add])
parseCommand ("Subtract":xs) ys = parseCommand xs (ys ++ [Subtract])
parseCommand ("Multiply":xs) ys = parseCommand xs (ys ++ [Multiply])
parseCommand ("Divide":xs) ys = parseCommand xs (ys ++ [Divide])

commands1 = parseCommands "Push 5 Pop Push 4.0"

variableMap :: Map.Map String Double
variableMap = Map.fromList [("x", 2.0), ("y", 3.0)]

expression :: FloatingExpr
expression = BinOpNode Expr.Add
    (BinOpNode Mul
        (Var "x")
        (Const 5)
    )
    (Const 1)

result :: Double
result = evalFloat variableMap expression

floatingExprStack = Map.fromList [
  (Expr.Add, Main.Add),
  (Sub, Subtract),
  (Mul, Multiply),
  (Div, Divide)
  ]

fromExprHelper (Const a) xs =  xs ++ [Push a]
fromExprHelper (Var a) xs = xs ++ [PushVar a]
fromExprHelper (BinOpNode binop expr1 expr2) xs 
  = (fromExprHelper expr2 (fromExprHelper expr1 xs)) ++ [floatingExprStack Map.! binop]

fromExpr expr = fromExprHelper expr []