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
  IDENTIFIER String |  STRING String |  NUMBER Int |

  --Keywords
  AND |  CLASS |  ELSE |  FALSE |  FUN |  FOR |  IF |  NIL |  OR |
  PRINT |  RETURN |  SUPER |  THIS |  TRUE |  VAR |  WHILE |

  EOF
  deriving Show

stripTilNewline :: String -> String
stripTilNewline "" = ""
stripTilNewline ('\n':xs) = xs
stripTilNewline (_:xs) = stripTilNewline xs

stripTilCommentEnd :: String -> String
stripTilCommentEnd "" = ""
stripTilCommentEnd ('*':'/':xs) = xs
stripTilCommentEnd (_:xs) = stripTilCommentEnd xs

append :: Token -> ([Token] -> Maybe [Token])
append x = (\xs -> Just (x:xs))

munchInt :: String -> String
munchInt "" = ""
munchInt xs =
  if isDigit (head xs)
    then head xs : munchInt (tail xs)
    else ""


munchNum :: String -> (Int,String)
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

tokenize :: String -> Maybe [Token]

tokenize "" = Just [EOF]
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
tokenize ('<':'=':xs) = tokenize xs >>= append LESS_EQUAL
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
    in tokenize (snd r) >>= append (IDENTIFIER (fst r))
  | otherwise = Nothing
  where
    h = head xs

main = print (tokenize "var foo = 6;if (foo == 1) {print \"bar\";}")
