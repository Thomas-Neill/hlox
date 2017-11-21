module Operators where
import Object
import Statement

truthiness :: LoxObject -> LoxObject
truthiness Nil = Boolean False
truthiness (Number 0) = Boolean False
truthiness (String "") = Boolean False
truthiness (Boolean x) = Boolean x
truthiness _ = Boolean True

failmsg :: String -> LoxObject -> LoxObject -> String
failmsg op a b = "Operation '" ++ op ++ "' is not supported with objects " ++ show a ++ " and " ++ show b ++ "."

add :: LoxObject -> LoxObject -> Either String LoxObject
add (Number n) (Number n') = return $ Number $ n + n'
add x y = return $ String $ (show x) ++ (show y)

mul :: LoxObject -> LoxObject -> Either String LoxObject
mul (Number n) (Number n') = return $ Number $ n * n'
mul (String s) (Number n) = return $ String $ concat $ replicate (round n) s
mul a b = Left $ failmsg "*" a b

sub :: LoxObject -> LoxObject -> Either String LoxObject
sub (Number n) (Number n') = return $ Number $ n - n'
sub x y = Left $ failmsg "-" x y

divide :: LoxObject -> LoxObject -> Either String LoxObject
divide (Number n) (Number 0) = Left "You can't divide by zero!"
divide (Number n) (Number n') = return $ Number $ n / n'
divide a b = Left $ failmsg "/" a b

eq :: LoxObject -> LoxObject -> Either String LoxObject
eq a b = return . Boolean $ a == b

neq :: LoxObject -> LoxObject -> Either String LoxObject
neq a b = return $ Boolean $ a /= b

gr :: LoxObject -> LoxObject -> Either String LoxObject
gr (Number n) (Number n') = return $ Boolean $ n > n'
gr a b = Left $ failmsg ">" a b

lt :: LoxObject -> LoxObject -> Either String LoxObject
lt (Number n) (Number n') = return $ Boolean $ n < n'
lt a b = Left $ failmsg "<" a b

greq :: LoxObject -> LoxObject -> Either String LoxObject
greq (Number n) (Number n') = return $ Boolean $ n >= n'
greq a b = Left $ failmsg ">=" a b

lteq :: LoxObject -> LoxObject -> Either String LoxObject
lteq (Number n) (Number n') = return $ Boolean $ n <= n'
lteq a b = Left $ failmsg "<=" a b

lookupBin :: BinOP -> Either String (LoxObject -> LoxObject -> Either String LoxObject)
lookupBin Plus = return add
lookupBin Minus = return sub
lookupBin Mul = return mul
lookupBin Div = return divide
lookupBin Equal = return eq
lookupBin Inequal = return neq
lookupBin Greater = return gr
lookupBin Less = return lt
lookupBin GrEqual = return greq
lookupBin LEqual = return lteq

loxNot :: LoxObject -> Either String LoxObject
loxNot x = return $ Boolean $ not $ toBool $ truthiness x

neg :: LoxObject -> Either String LoxObject
neg (Number n) = return $ Number $ negate n
neg _ = Left "Operation '-' (unary) is only supported for numbers"

lookupUn :: UnOP -> Either String (LoxObject -> Either String LoxObject)
lookupUn Not = return loxNot
lookupUn Negate = return neg
