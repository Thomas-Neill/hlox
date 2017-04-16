module Scanner where
import Data.Char
data Token =
  --One character tokens
  LEFT_PAREN |  RIGHT_PAREN |  LEFT_BRACE |  RIGHT_BRACE |
  COMMA |  DOT |  MINUS |  PLUS |  SEMICOLON |  SLASH |  STAR |

  --One or two char tokens
  BANG |  BANG_EQUAL |
  EQUAL |  EQUAL_EQUAL |
  GREATER |  GREATER_EQUAL |
  LESS |  LESS_EQUAL |

  --literals and user-defined names
  IDENTIFIER String |  STRING String |  NUMBER Double |

  --Keywords
  AND |  CLASS |  ELSE |  FALSE |  FUN |  FOR |  IF |  NIL |  OR |
  PRINT |  RETURN |  SUPER |  THIS |  TRUE |  VAR |  WHILE | EOF
  deriving Eq

instance Show Token where
  show LEFT_PAREN = "("
  show RIGHT_PAREN = ")"
  show LEFT_BRACE = "{"
  show RIGHT_BRACE = "}"
  show COMMA = ","
  show DOT = "."
  show MINUS = "-"
  show PLUS = "+"
  show SEMICOLON = ";"
  show SLASH = "/"
  show STAR = "*"
  show BANG = "!"
  show BANG_EQUAL = "!="
  show EQUAL = "="
  show EQUAL_EQUAL = "=="
  show GREATER = ">"
  show GREATER_EQUAL = ">="
  show LESS = "<"
  show LESS_EQUAL = "<="
  show (IDENTIFIER s) = s
  show (STRING s) = "\"" ++ s ++ "\""
  show (NUMBER i) = show i
  show AND = "and"
  show CLASS = "class"
  show ELSE = "else"
  show FALSE = "false"
  show FUN = "fun"
  show FOR = "for"
  show IF = "if"
  show NIL = "nil"
  show OR = "or"
  show PRINT = "print"
  show RETURN = "return"
  show SUPER = "super"
  show THIS = "this"
  show TRUE = "true"
  show VAR = "var"
  show WHILE = "while"
  show EOF = "EOF"

stripTilNewline :: String -> String
stripTilNewline "" = ""
stripTilNewline ('\n':xs) = xs
stripTilNewline (_:xs) = stripTilNewline xs

stripTilCommentEnd :: String -> String
stripTilCommentEnd "" = ""
stripTilCommentEnd ('*':'/':xs) = xs
stripTilCommentEnd (_:xs) = stripTilCommentEnd xs

munchInt :: String -> String
munchInt "" = ""
munchInt xs =
  if isDigit (head xs) || (head xs) == '.'
    then head xs : munchInt (tail xs)
    else ""


munchNum :: String -> (Double,String)
munchNum xs =
  let
    result = munchInt xs
    remaining = drop (length result) xs
  in (read result,remaining)

munchStrLit :: String -> (String,String)
munchStrLit ('"':xs) = ("",xs)
munchStrLit (x:xs) =
  let
    (result,leftovers) = munchStrLit xs
  in (x:result,leftovers)

isVarChar :: Char -> Bool
isVarChar ch = ch `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

munchVar :: String -> (String,String)
munchVar "" = ("","")
munchVar xs =
  if isVarChar (head xs)
  then let
    (result,leftovers) = munchVar (tail xs)
  in (head xs:result,leftovers)
  else
    ("",xs)

getKeyword :: Token -> Token
getKeyword (IDENTIFIER "and") = AND
getKeyword (IDENTIFIER "class") = CLASS
getKeyword (IDENTIFIER "else") = ELSE
getKeyword (IDENTIFIER "false") = FALSE
getKeyword (IDENTIFIER "fun") = FUN
getKeyword (IDENTIFIER "for") = FOR
getKeyword (IDENTIFIER "if") = IF
getKeyword (IDENTIFIER "nil") = NIL
getKeyword (IDENTIFIER "print") = PRINT
getKeyword (IDENTIFIER "return") = RETURN
getKeyword (IDENTIFIER "super") = SUPER
getKeyword (IDENTIFIER "this") = THIS
getKeyword (IDENTIFIER "true") = TRUE
getKeyword (IDENTIFIER "var") = VAR
getKeyword (IDENTIFIER "while") = WHILE
getKeyword x = x

append :: (Monad m) => Token -> ([Token] -> m [Token])
append x = (\xs -> return (x:xs))

tokenize :: (Monad m) => String -> m [Token]

tokenize "" = return [EOF]
tokenize (' ':xs)     = tokenize xs
tokenize ('\n':xs)    = tokenize xs
tokenize ('\t':xs)    = tokenize xs
tokenize ('(':xs)     = tokenize xs >>= append LEFT_PAREN
tokenize (')':xs)     = tokenize xs >>= append RIGHT_PAREN
tokenize ('{':xs)     = tokenize xs >>= append LEFT_BRACE
tokenize ('}':xs)     = tokenize xs >>= append RIGHT_BRACE
tokenize (',':xs)     = tokenize xs >>= append COMMA
tokenize ('.':xs)     = tokenize xs >>= append DOT
tokenize ('-':xs)     = tokenize xs >>= append MINUS
tokenize ('+':xs)     = tokenize xs >>= append PLUS
tokenize ('*':xs)     = tokenize xs >>= append STAR
tokenize (';':xs)     = tokenize xs >>= append SEMICOLON
tokenize ('/':'/':xs) = tokenize (stripTilNewline xs)
tokenize ('/':'*':xs) = tokenize (stripTilCommentEnd xs)
tokenize ('/':xs)     = tokenize xs >>= append SLASH
tokenize ('!':'=':xs) = tokenize xs >>= append BANG_EQUAL
tokenize ('!':xs)     = tokenize xs >>= append BANG
tokenize ('=':'=':xs) = tokenize xs >>= append EQUAL_EQUAL
tokenize ('=':xs)     = tokenize xs >>= append EQUAL
tokenize ('>':'=':xs) = tokenize xs >>= append GREATER_EQUAL
tokenize ('>':xs)     = tokenize xs >>= append GREATER
tokenize ('<':'=':xs) = tokenize xs >>= append LESS_EQUAL
tokenize ('<':xs)     = tokenize xs >>= append LESS
tokenize ('"':xs) =
  let
    (strLit,remaining) = munchStrLit xs
  in tokenize remaining >>= append (STRING strLit)
tokenize xs
  | isDigit h =
    let
      r = munchNum xs
    in tokenize (snd r) >>= append (NUMBER (fst r))
  | isVarChar h =
    let
      r = munchVar xs
    in tokenize (snd r) >>= append (getKeyword (IDENTIFIER (fst r)))
  | otherwise = fail ("Invalid char: "++[h])
  where
    h = head xs
