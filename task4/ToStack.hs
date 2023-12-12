import Parser
import Stack

translateSumToCommands :: Sum -> [Command]
translateSumToCommands (Sum prod []) = translateProdToCommands prod
translateSumToCommands (Sum prod rest) =
  translateProdToCommands prod ++ translateRestToCommands rest

translateProdToCommands :: Prod -> [Command]
translateProdToCommands (Prod term []) = translateTermToCommands term
translateProdToCommands (Prod term rest) =
  translateTermToCommands term ++ translateMulRestToCommands rest

translateTermToCommands :: Term -> [Command]
translateTermToCommands (Numeral x) = [Push (fromIntegral x)]
translateTermToCommands (Subexpr sum) = translateSumToCommands sum
translateTermToCommands (Variable name) = [PushVar name]

translateRestToCommands :: [(AddOp, Prod)] -> [Command]
translateRestToCommands [] = []
translateRestToCommands ((op, prod):rest) =
  translateProdToCommands prod ++ translateRestToCommands rest ++ [opCommand]
  where
    opCommand = case op of
      Parser.Add -> Stack.Add
      Sub -> Subtract

translateMulRestToCommands :: [(MulOp, Term)] -> [Command]
translateMulRestToCommands [] = []
translateMulRestToCommands ((op, term):rest) =
  translateTermToCommands term ++ translateMulRestToCommands rest ++ [opCommand]
  where
    opCommand = case op of
      Mul -> Multiply
      Div -> Divide
