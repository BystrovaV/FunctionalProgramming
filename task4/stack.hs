module Stack where
import Data.List (foldl)
import qualified Data.Map as Map


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

executeCommand Add state = state { stack = (x + y) : rest }
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
parseCommand ("Add":xs) ys = parseCommand xs (ys ++ [Add])
parseCommand ("Subtract":xs) ys = parseCommand xs (ys ++ [Subtract])
parseCommand ("Multiply":xs) ys = parseCommand xs (ys ++ [Multiply])
parseCommand ("Divide":xs) ys = parseCommand xs (ys ++ [Divide])

commands1 = parseCommands "Push 5 Pop Push 4.0"
