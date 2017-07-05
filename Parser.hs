module Parser where
import Scanner
import Object

type Parser a = [Token] -> Either String (a,[Token])

data Expr = Literal LoxObject |
            Unary Token Expr |
            Grouping Expr |
            Binary Expr Token Expr |
            Variable LValue |
            Assignment LValue Expr |
            InlineIf Expr Expr Expr

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
  show (InlineIf c i e) = "(if " ++ show c ++ " " ++ show i ++ " " ++ show e ++ ")"

data Statement = Empty |
                Expression Expr |
                Print Expr |
                Declaration LValue Expr |
                Compound [Statement] |
                If Expr Statement Statement |
                While Expr Statement

instance Show Statement where
  show (Expression e) = show e ++ ";"
  show (Print e) = "print " ++ show e ++ ";"
  show (Declaration l e) = "var " ++ show l ++ " = " ++ show e ++ ";"
  show (Compound exprs) = foldl (++) "{" (map show exprs) ++ "}"
  show Empty = ";"
  show (If expr i e) = "if(" ++ show expr ++ ") " ++ show i ++ " else " ++ show e
  show (While expr st) = "while " ++ show expr ++ " " ++ show st

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
lvalue xs = Left ("Expected literal or identifier: " ++ (show (head xs)))

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
  (Left _) -> equality xs --we couldn't parse an lvalue
  (Right (name,EQUAL:xs')) -> do
    (value,xs'') <- assignment xs'
    return $ (Assignment name value,xs'')
  (Right _) -> equality xs

inlineif :: Parser Expr
inlineif (IF:xs) = do
  result <- assignment xs
  case result of
    (cond,THEN:xs') -> do
      result' <- assignment xs'
      case result' of
        (thn,ELSE:xs'') -> do
          (els,xs''') <- assignment xs''
          return (InlineIf cond thn els,xs''')
        _ -> Left "expected else in inline if"
    _ -> Left "expected then in inline if"
inlineif xs = assignment xs

expression :: Parser Expr
expression = inlineif

statement :: Parser Statement
statement (PRINT:xs) = do
  case expression xs of
    (Left errmsg) -> Left errmsg
    (Right (expr,SEMICOLON:xs')) -> return (Print expr,xs')
    (Right (_,x:xs)) -> Left $ "Expected semicolon but got '" ++ show x ++ "'"
statement (VAR:xs) = do
  result <- lvalue xs
  case result of
    (name,SEMICOLON:xs') -> return (Declaration name (Literal Nil),xs')
    (name,EQUAL:xs') -> do
      (expr,SEMICOLON:xs'') <- expression xs'
      return (Declaration name expr,xs'')
statement (LEFT_BRACE:xs) = do
  (result,xs') <- aux xs
  return $ (Compound result,xs')
  where
  aux :: Parser [Statement]
  aux [EOF] = Left "Unterminated compound statement."
  aux (RIGHT_BRACE:xs) = return ([],xs)
  aux xs = do
    (result,xs') <- statement xs
    (rest,xs'') <- aux xs'
    return (result:rest,xs'')
statement (SEMICOLON:xs) = return (Empty,xs)
statement (IF:LEFT_PAREN:xs) = do
  (cond,RIGHT_PAREN:xs') <- expression xs
  (if',xs'') <- statement xs'
  case xs'' of
    (ELSE:xs'') -> do
      (else',xs''') <- statement xs''
      return (If cond if' else',xs''')
    (_) -> return (If cond if' Empty,xs'')
statement (WHILE:xs) = do
  (cond,xs') <- expression xs
  (stm,xs'') <- statement xs'
  return $ (While cond stm,xs'')
statement (FOR:LEFT_PAREN:xs) = do
  (initalizer,xs') <- statement xs
  (check,SEMICOLON:xs'') <- expression xs'
  (eachloop,RIGHT_PAREN:xs''') <- statement xs''
  (body,xs'''') <- statement xs'''
  return $ (Compound [initalizer,While check (Compound [body,eachloop])],xs'''')
statement xs = do
  case expression xs of
    (Left errmsg) -> Left errmsg
    (Right (expr,SEMICOLON:xs')) -> return (Expression expr,xs')
    (Right (_,x:xs)) -> Left $ "Expected semicolon but got '" ++ show x ++ "'"

parse :: [Token] -> Either String [Statement]
parse [EOF] = return []
parse xs = do
  (stmt,xs') <- statement xs
  rest <- parse xs'
  return $ stmt:rest

prettyprint :: Either String [Statement] -> String
prettyprint (Left err) = "Error: " ++ err
prettyprint (Right (line:xs)) = show line ++ "\n" ++ prettyprint (Right xs)
prettyprint (Right []) = ""
