module Parser where
import Scanner
import Object
import Result

type Parser a = [Token] -> Result (a,[Token])

data Expr = Literal LoxObject |
            Unary Token Expr |
            Grouping Expr |
            Binary Expr Token Expr |
            Variable LValue |
            Assignment LValue Expr

data LValue = Name String

instance Show LValue where
  show (Name s) = '#':s

instance Show Expr where
  show (Literal l) = show l
  show (Unary t e) = "(" ++ show t ++ " " ++ show e ++ ")"
  show (Grouping e) = "(group " ++ show e ++ ")"
  show (Binary r t l) = "(" ++ show t ++ " " ++ show r ++ " " ++ show l ++ ")"
  show (Variable l) = show l
  show (Assignment l v) = "(set " ++ show l ++ " " ++ show v ++ ")"

data Statement = Expression Expr | Print Expr | Declaration LValue Expr

instance Show Statement where
  show (Expression e) = show e ++ ";"
  show (Print e) = "print " ++ show e ++ ";"
  show (Declaration l e) = "var " ++ show l ++ " = " ++ show e ++ ";"

primary :: Parser Expr
primary ((NUMBER n):xs) = return (Literal (Number n),xs)
primary ((STRING x):xs) = return (Literal (String x),xs)
primary (FALSE:xs)      = return (Literal (Boolean False),xs)
primary (TRUE:xs)       = return (Literal (Boolean True),xs)
primary (NIL:xs)        = return (Literal Nil,xs)
primary (LEFT_PAREN:xs) = do
  (result,RIGHT_PAREN:remainder) <- equality xs
  return (Grouping result,remainder)
primary xs = do
  (result,remainder) <- lvalue xs
  return (Variable result,remainder)

lvalue :: Parser LValue
lvalue (IDENTIFIER name:xs) = return (Name name,xs)
lvalue xs = fail ("Expected literal or identifier: " ++ (show (head xs)))

unary :: Parser Expr
unary (BANG:xs) = do
  (result,remainder) <- unary xs
  return (Unary BANG result,remainder)
unary (MINUS:xs) = do
  (result,remainder) <- unary xs
  return (Unary MINUS result,remainder)
unary xs = primary xs

generateRule :: Parser Expr -> [Token] -> Parser Expr
generateRule precedent munches tokens = do
  (result,remainder) <- precedent tokens
  (chunks,remainder') <- chunk precedent munches remainder
  return (foldr (\x acc -> Binary acc (fst x) (snd x)) result chunks,remainder')
  where
    chunk :: (Monad m) =>  ([Token] -> m (Expr,[Token])) -> [Token] -> [Token] -> m ([(Token,Expr)],[Token])
    chunk precedent munches tokens =
      if (head tokens) `elem` munches then do
        (result,remainder) <- precedent (tail tokens)
        (result',remainder') <- chunk precedent munches remainder
        return ((head tokens,result):result',remainder')
      else
        return ([],tokens)

factor :: Parser Expr
factor = generateRule unary [SLASH,STAR]

term :: Parser Expr
term = generateRule factor [PLUS,MINUS]

comparison :: Parser Expr
comparison = generateRule term [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL]

equality :: Parser Expr
equality = generateRule comparison [BANG_EQUAL,EQUAL_EQUAL]

assignment :: Parser Expr
assignment xs = case lvalue xs of
  (Failure _) -> equality xs --we couldn't parse an lvalue
  (Result (name,EQUAL:xs')) -> do
    (value,xs'') <- assignment xs'
    return $ (Assignment name value,xs'')
  (Result _) -> equality xs

expression :: Parser Expr
expression = assignment

statement :: Parser Statement
statement (PRINT:xs) = do
  (expr,SEMICOLON:xs') <- expression xs
  return (Print expr,xs')
statement (VAR:xs) = do
  result <- lvalue xs
  case result of
    (name,SEMICOLON:xs') -> return (Declaration name (Literal Nil),xs')
    (name,EQUAL:xs') -> do
      (expr,SEMICOLON:xs'') <- expression xs'
      return (Declaration name expr,xs'')
statement xs = do
  (expr,SEMICOLON:xs') <- expression xs
  return (Expression expr,xs')

parse :: [Token] -> Result [Statement]
parse [EOF] = return []
parse xs = do
  (stmt,xs') <- statement xs
  rest <- parse xs'
  return $ stmt:rest
