module Parser where
import Statement
import Object
import Text.Parsec hiding (Empty)
import Text.ParserCombinators.Parsec hiding (try)

num :: Parser Double
num = read <$> many1 (digit <|> char '.')

str :: Parser [Char]
str = char '"' *> (many $ noneOf ['"']) <* char '"'

spaces1 = many1 space

pad :: Parser a -> Parser a
pad parser = spaces *> parser <* spaces

genNext :: [(String,BinOP)] -> Parser Expr -> Parser Expr
genNext mp pred = pred `chainl1` getOp <?> "expression"
  where
    b' op x y = Binary x op y
    getOp = (try . pad $ (b' . lookup') <$> (choice (map (try . string . fst) mp)))
    lookup' x = case lookup x mp of
                      Nothing -> error "Impossible"
                      Just r -> r

lvalue = Name <$> varName <?> "variable"
varName = (:) <$> oneOf alphabet <*> many varChar
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
varChar = oneOf $ alphabet ++ "0123456789"

primary = choice [
    Literal . Number <$> num,
    Literal . String <$> str,
    try $ Literal . Boolean . (== "true") <$> (string "false" <|> string "true"),
    try $ Literal . const Nil <$> string "nil",
    try $ Rocket <$> varName <*> ((pad $ string "=>") *> expr),
    Variable <$> lvalue,
    Grouping <$> (char '(' *> expr <* char ')')] <?> "literal, variable or parenthesized expression"

unary = choice [
  Unary Not <$> (char '!' *> spaces *> unary),
  Unary Negate <$> (char '-' *> spaces *> unary),
  primary] <?> "unary expression"

funcall' = combine <$> primary <*> many1 argList
  where
    argList = char '(' *>
      (
        (++) <$> many (try $ expr <* char ',') <*> fmap (:[]) expr <|> pure []
      )
      <* char ')'
    combine = foldl Funcall

funcall = (try funcall') <|> unary

factor = genNext [("*",Mul),("/",Div)] funcall
term = genNext [("+",Plus),("-",Minus)] factor
comparison = genNext [(">=",GrEqual),("<=",LEqual),(">",Greater),("<",Less)] term
equality = genNext [("==",Equal),("!=",Inequal)] comparison

assignment = Assignment <$> lvalue <*> (spaces *> string "=" *> expr)

inlineif = InlineIf <$> (string "if" *> spaces1 *> expr' <* spaces1)
                    <*> (string "then" *> spaces1 *> expr' <* spaces1)
                    <*> (string "else" *> spaces1 *> expr')

expr' = choice [try inlineif,try assignment,try equality]
expr = pad expr'

statement = (pad $ choice [
  try $ Print <$> (string "print" *> spaces1 *> expr <* char ';'),
  char ';' *> pure Empty,
  try $ Declaration <$> (string "var" *> spaces1 *> lvalue) <*>
                  (option (Literal Nil) $ spaces *> char '=' *> expr)
                  <* char ';',
  Compound <$> (char '{' *> many statement <* char '}'),
  try $ If <$> (string "if(" *> expr) <*>
         (char ')' *> statement) <*>
         (option Empty $ string "else" *> statement),
  try $ While <$> (string "while" *> spaces1 *> expr <* spaces1) <*> statement,
  try $ for <$> (string "for(" *> statement) <*>
        (expr <* char ';') <*>
        (statement <* char ')') <*>
        statement,
  try $ string "break;" *> pure Break,
  Expression <$> expr <* char ';'])

parsed = parse $ pad $ many statement <* eof
