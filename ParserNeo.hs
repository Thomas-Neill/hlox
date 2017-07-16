module ParserNeo where
import Statement
import Object
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import qualified Parser (expression)
import qualified Scanner (tokenize)

num :: Parser Double
num = read <$> many1 (digit <|> char '.')

str :: Parser [Char]
str = char '"' *> (many $ noneOf ['"']) <* char '"'

whitespace = oneOf " \t\n"
spaces1 = many1 whitespace

pad :: Parser a -> Parser a
pad parser = spaces *> parser <* spaces

genNext :: [(String,BinOP)] -> Parser Expr -> Parser Expr
genNext mp pred = pred `chainl1` getOp
  where
    b' op x y = Binary x op y
    getOp = try . pad $ (b' . lookup') <$> (choice (map (string . fst) mp))
    lookup' x = case lookup x mp of
                      Nothing -> error "Impossible"
                      Just r -> r

primary = (choice [
    Literal . Number <$> num,
    Literal . String <$> str,
    try $ Literal . Boolean . (== "true") <$> (string "false" <|> string "true"),
    try $ Literal . const Nil <$> string "nil",
    fmap Variable lvalue,
    Grouping <$> (char '(' *> expr <* char ')')] <?> "literal, variable or parenthesized expression")

lvalue = Name <$> many1 varChar
  where
    varChar = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

unary = choice [
  Unary Not <$> (char '!' *> spaces *> unary),
  Unary Negate <$> (char '-' *> spaces *> unary),
  primary]

factor = genNext [("*",Mul),("/",Div)] unary
term = genNext [("+",Plus),("-",Minus)] factor
comparison = genNext [(">=",GrEqual),("<=",LEqual)] term
equality = genNext [("==",Equal),("!=",Inequal)] comparison

assignment = Assignment <$> lvalue <*> (string "=" *> expr)

inlineif = InlineIf <$> (string "if" *> spaces1 *> expr' <* spaces1)
                    <*> (string "then" *> spaces1 *> expr' <* spaces1)
                    <*> (string "else" *> spaces1 *> expr')

expr' = choice [try inlineif,try assignment,try equality]
expr = pad expr'

main = do
  inp <- getLine
  let result = parse expr "" inp
  putStrLn "New parser: "
  print result
  putStrLn "Old parser: "
  print . fmap fst $ Scanner.tokenize inp >>= Parser.expression
