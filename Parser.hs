module Parser where
import Scanner
import Debug.Trace

data LoxObject = String {toString::String} | Number {toDouble::Double} | Boolean {toBool::Bool} | Nil deriving Eq

instance Show LoxObject where
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (Boolean b) = show b
  show Nil = "nil"

data Expr = Literal LoxObject | Unary Token Expr | Grouping Expr | Binary Expr Token Expr

instance Show Expr where
  show (Literal l) = show l
  show (Unary t e) = "(" ++ show t ++ " " ++ show e ++ ")"
  show (Grouping e) = "(group " ++ show e ++ ")"
  show (Binary r t l) = "(" ++ show t ++ " " ++ show r ++ " " ++ show l ++ ")"

parse :: [Token] -> Either String Expr
parse xs = do
  (result,[EOF]) <- equality xs
  return result

primary :: [Token] -> Either String (Expr,[Token])
primary ((NUMBER n):xs) = return (Literal (Number n),xs)
primary ((STRING x):xs) = return (Literal (String x),xs)
primary (FALSE:xs)      = return (Literal (Boolean False),xs)
primary (TRUE:xs)       = return (Literal (Boolean True),xs)
primary (NIL:xs)        = return (Literal Nil,xs)
primary (LEFT_PAREN:xs) = do
  (result,RIGHT_PAREN:remainder) <- equality xs
  return (Grouping result,remainder)
primary xs = fail ("Expected literal or identifier: " ++ (show (head xs)))

unary :: [Token] -> Either String (Expr,[Token])
unary (BANG:xs) = do
  (result,remainder) <- unary xs
  return (Unary BANG result,remainder)
unary (MINUS:xs) = do
  (result,remainder) <- unary xs
  return (Unary MINUS result,remainder)
unary xs = primary xs


generateRule ::  ([Token] -> Either String (Expr,[Token]) ) -> [Token] -> [Token] -> Either String (Expr,[Token])
generateRule precedent munches tokens = do
  (result,remainder) <- precedent tokens
  if (head remainder) `elem` munches then do
    (result',remainder') <- generateRule precedent munches (tail remainder)
    return (Binary result (head remainder) result',remainder')
  else
    return (result,remainder)

factor = generateRule unary [SLASH,STAR]
term = generateRule factor [PLUS,MINUS]
comparison = generateRule term [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL]
equality = generateRule comparison [BANG_EQUAL,EQUAL_EQUAL]
