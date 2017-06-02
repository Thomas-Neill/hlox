module Parser where
import Scanner
import Object

data Expr = Literal LoxObject | Unary Token Expr | Grouping Expr | Binary Expr Token Expr

instance Show Expr where
  show (Literal l) = show l
  show (Unary t e) = "(" ++ show t ++ " " ++ show e ++ ")"
  show (Grouping e) = "(group " ++ show e ++ ")"
  show (Binary r t l) = "(" ++ show t ++ " " ++ show r ++ " " ++ show l ++ ")"

data Statement = Expression Expr | Print Expr

instance Show Statement where
  show (Expression e) = show e ++ ";"
  show (Print e) = "print " ++ show e ++ ";"

primary :: (Monad m) => [Token] -> m (Expr,[Token])
primary ((NUMBER n):xs) = return (Literal (Number n),xs)
primary ((STRING x):xs) = return (Literal (String x),xs)
primary (FALSE:xs)      = return (Literal (Boolean False),xs)
primary (TRUE:xs)       = return (Literal (Boolean True),xs)
primary (NIL:xs)        = return (Literal Nil,xs)
primary (LEFT_PAREN:xs) = do
  (result,RIGHT_PAREN:remainder) <- equality xs
  return (Grouping result,remainder)
primary xs = fail ("Expected literal or identifier: " ++ (show (head xs)))

unary :: (Monad m) => [Token] -> m (Expr,[Token])
unary (BANG:xs) = do
  (result,remainder) <- unary xs
  return (Unary BANG result,remainder)
unary (MINUS:xs) = do
  (result,remainder) <- unary xs
  return (Unary MINUS result,remainder)
unary xs = primary xs

generateRule :: (Monad m) =>  ([Token] -> m (Expr,[Token])) -> [Token] -> [Token] -> m (Expr,[Token])
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

factor :: (Monad m) => [Token] -> m (Expr,[Token])
factor = generateRule unary [SLASH,STAR]

term :: (Monad m) => [Token] -> m (Expr,[Token])
term = generateRule factor [PLUS,MINUS]

comparison :: (Monad m) => [Token] -> m (Expr,[Token])
comparison = generateRule term [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL]

equality :: (Monad m) => [Token] -> m (Expr,[Token])
equality = generateRule comparison [BANG_EQUAL,EQUAL_EQUAL]

expression :: (Monad m) => [Token] -> m (Expr,[Token])
expression = equality

statement :: (Monad m) => [Token] -> m (Statement,[Token])
statement (PRINT:xs) = do
  (expr,SEMICOLON:xs') <- expression xs
  return (Print expr,xs')
statement xs = do
  (expr,SEMICOLON:xs') <- expression xs
  return (Expression expr,xs')

parse :: (Monad m) => [Token] -> m [Statement]
parse [EOF] = return []
parse xs = do
  (stmt,xs') <- statement xs
  rest <- parse xs'
  return $ stmt:rest
