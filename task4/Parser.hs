module Parser where
import Data.Char
import Data.Functor (void)
import Data.Typeable
import Control.Applicative

-- принимает строку, возвращает что-то и часть строки
newtype Parser a = Parser
  { runParser :: String -> [(a, String)] }

-- принимает функцию и парсер, возвращает новый парсер,
-- который применяет f к результату парсера q
instance Functor Parser where
  fmap f q = Parser h where
    h s = [(f y, s') | (y, s') <- runParser q s]

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  Parser p <*> Parser q = Parser h where
    h s = [(f y, s'') | (f, s') <- p s, (y, s'') <- q s']

instance Alternative Parser where
  empty = Parser (const [])
  Parser p <|> Parser q = Parser h where
    h s = p s ++ q s

acceptIf :: (Char -> Bool) -> Parser Char
acceptIf p = Parser f where
  f (c:cs) | p c = [(c, cs)]
  f _ = []

acceptChar c = acceptIf (==c)

letter :: Parser Char
letter = acceptIf isAlpha

digit :: Parser Char
digit = acceptIf isDigit

blank = acceptIf isSpace

ignoreIf :: (Char -> Bool) -> Parser ()
ignoreIf p = void (acceptIf p)

ignoreChar c = ignoreIf (==c)

times :: Int -> Parser a -> Parser [a]
times 0 _ = pure []
times n p = liftA2 (:) p (times (n-1) p)

upTo :: Int -> Parser a -> Parser [a]
upTo 0 _ = pure []
upTo n p = (liftA2 (:) p (upTo (n-1) p)) <|> upTo 0 p

fromTo :: Int -> Int -> Parser a -> Parser [a]
fromTo m n p = liftA2 (++) (times m p) (upTo (n-m) p)

-- <Expr>       ::= <Sum>
-- <Sum>        ::= <Prod> { <AddOp> <Prod> }
-- <Prod>       ::= <Term> { <MulOp> <Term> }
-- <Term>       ::= <Numeral> | '(' <Sum> ')'
-- <AddOp>      ::= '+' | '-'
-- <MulOp>      ::= '*' | '/'
-- <Numeral>    ::= <Digit> { <Digit> }

data AddOp = Add | Sub deriving (Show)
data MulOp = Mul | Div deriving (Show)
data Sum = Sum Prod [(AddOp, Prod)] deriving (Show)
data Prod = Prod Term [(MulOp, Term)] deriving (Show)
data Term = Numeral Int | Subexpr Sum | Variable String deriving (Show)

addOpE :: Parser AddOp
addOpE = (const Add <$> acceptChar '+') <|> (const Sub <$> acceptChar '-')

mulOpE :: Parser MulOp
mulOpE = (const Mul <$> acceptChar '*') <|> (const Div <$> acceptChar '/')

sumE :: Parser Sum
sumE = liftA2 Sum prodE (many (liftA2 (,) addOpE prodE) )

prodE :: Parser Prod
prodE = liftA2 Prod termE (many (liftA2 (,) mulOpE termE))

termE :: Parser Term
termE = (Numeral . read <$> some digit) <|> (liftA3 (\_ x _ -> Subexpr x) (ignoreChar '(') sumE (ignoreChar ')')) <|> (Variable <$> some letter)

parseSum = requireComplete . head . runParser sumE where
    requireComplete (x, "") = x

-- evalSum :: Sum -> Float
-- evalSum (Sum a bs) = (evalProd a) + (sum $ map f bs) where
--     f (op, b) = g op $ evalProd b
--     g Add = id
--     g Sub = negate

-- evalProd :: Prod -> Float
-- evalProd (Prod a bs) = evalTerm a * (product $ map f bs) where
--     f (op, b) = g op $ evalTerm b
--     g Mul = id
--     g Div = recip

-- evalTerm :: Term -> Float
-- evalTerm (Numeral x) = fromIntegral x
-- evalTerm (Subexpr x) = evalSum x

