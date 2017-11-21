module Parser where
import Statement
import Object
import Text.Parsec hiding (Empty)
import Text.ParserCombinators.Parsec hiding (try)

--helpers
num :: Parser Double
num = fmap read $ (++) <$> many1 digit <*> (option "" $ (:) <$> char '.' <*> many1 digit)

str :: Parser [Char]
str = char '"' *> (many $ noneOf ['"']) <* char '"'

tstring = try . string

whitespace = oneOf " \t\n"
spaces1 = many1 whitespace

pad :: Parser a -> Parser a
pad parser = spaces *> parser <* spaces

genNext :: [(String,BinOP)] -> Parser Expr -> Parser Expr
genNext mp pred = pred `chainl1` getOp
  where
    b' op x y = Binary x op y
    getOp = (try . pad $ (b' . lookup') <$> (choice (map (try . string . fst) mp)))
    lookup' x = case lookup x mp of
                      Nothing -> error "Impossible"
                      Just r -> r

varName = (:) <$> oneOf alphabet <*> many varChar
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
varChar = oneOf $ alphabet ++ "0123456789"

argListOf :: Parser a -> Parser [a]
argListOf x = char '(' *>
            pad ((pad x) `sepBy` char ',')
          <* char ')'

--parsers
--TODO: Right now, the parsers are a spaghettified mess.
-- I need to write a cfg and a parser conforming to it.


var = Name <$> varName

primary' = choice [
    Literal . Number <$> num,
    Literal . String <$> str,
    Literal . Boolean . (== "true") <$> (tstring "false" <|> tstring "true"),
    Literal . const Nil <$> tstring "nil",
    Rocket <$> (char '[' *> spaces *> varName) <*> ((pad $ string "=>") *> expr <* char ']'),
    Fun <$> (tstring "fun" *> spaces *> argListOf varName) <*> (spaces *> char '{' *> many statement <* char '}'),
    Variable <$> var,
    CreateObject <$> (
      char '|' *> spaces *>
        ((,) <$> (spaces *> varName) <*> (spaces *> char ':' *> expr)) `sepBy` (char ',')
      <* spaces <* char '|'),
    Grouping <$> (char '(' *> expr <* char ')')]

access = do
  left <- primary'
  (a:as) <- many (char '.' *> varName)
  return $ foldl (\acc x -> Access (Variable acc) x) (Access left a) as

primary = (try $ Variable <$> access) <|> primary'

lval = (try access) <|> var

unary = choice [
  Unary Not <$> (char '!' *> spaces *> unary),
  Unary Negate <$> (char '-' *> spaces *> unary),
  primary]

factor = genNext [("*",Mul),("/",Div)] unary
term = genNext [("+",Plus),("-",Minus)] factor
comparison = genNext [(">=",GrEqual),("<=",LEqual),(">",Greater),("<",Less)] term
equality = genNext [("==",Equal),("!=",Inequal)] comparison

assignment = Assignment <$> lval <*> (spaces *> string "=" *> expr)

inlineif = InlineIf <$> (string "if" *> spaces1 *> expr' <* spaces1)
                    <*> (string "then" *> spaces1 *> expr' <* spaces1)
                    <*> (string "else" *> spaces1 *> expr')

funcall = combine <$> primary <*> many1 argList
    where
      argList = argListOf expr
      combine = foldl Funcall

expr' = choice [try inlineif, try assignment, try funcall, equality]
expr = pad expr'

statement = pad $ choice [
  Print <$> (tstring "print" *> spaces1 *> expr <* char ';'),
  Declaration <$> (tstring "var" *> spaces1 *> var) <*>
                  (option (Literal Nil) $ spaces *> char '=' *> expr)
                  <* char ';',
  If <$> (tstring "if(" *> expr) <*>
         (char ')' *> statement) <*>
         (option Empty $ string "else" *> statement),
  While <$> (tstring "while" *> char '(' *> expr <* char ')') <*> statement,
  for <$> (tstring "for(" *> statement) <*>
        (expr <* char ';') <*>
        (statement <* char ')') <*>
        statement,
  tstring "break;" *> pure Break,
  Return <$> (tstring "return;" *> pure (Literal Nil)),
  Return <$> (tstring "return" *> spaces1 *> expr <* char ';'),
  defunc <$>
    (tstring "def" *> spaces1 *> varName ) <*>
    (spaces *> argListOf varName <* spaces) <*>
    (char '{' *> many statement <* char '}'),
  Compound <$> (char '{' *> many statement <* char '}'),
  Expression <$> expr <* char ';',
  char ';' *> pure Empty]

parsed = parse $ pad $ many statement <* eof

parsedF = parseFromFile (pad $ many statement <* eof)
