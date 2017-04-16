module Operators where
import Parser
import Scanner

truthiness :: LoxObject -> LoxObject
truthiness Nil = Boolean False
truthiness (Number 0) = Boolean False
truthiness (String "") = Boolean False
truthiness _ = Boolean True

failmsg :: String -> LoxObject -> LoxObject -> String
failmsg op a b = "Operation '" ++ op ++ "' is not supported with objects " ++ show a ++ " and " ++ show b ++ "."

add :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
add (Number n) (Number n') = return $ Number $ n + n'
add (String s) (String s') = return $ String $ s ++ s'
add a b = fail $ failmsg "+" a b

mul :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
mul (Number n) (Number n') = return $ Number $ n * n'
mul (String s) (Number n) = return $ String $ concat $ replicate (round n) s
mul a b = fail $ failmsg "*" a b

sub :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
sub (Number n) (Number n') = return $ Number $ n - n'

divide :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
divide (Number n) (Number 0) = fail "You can't divide by zero!"
divide (Number n) (Number n') = return $ Number $ n / n'
divide a b = fail $ failmsg "/" a b

eq :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
eq a b = return $ Boolean $ a == b

neq :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
neq a b = return $ Boolean $ a /= b

gr :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
gr (Number n) (Number n') = return $ Boolean $ n > n'
gr a b = fail $ failmsg ">" a b

lt :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
lt (Number n) (Number n') = return $ Boolean $ n < n'
lt a b = fail $ failmsg "<" a b

greq :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
greq (Number n) (Number n') = return $ Boolean $ n >= n'
greq a b = fail $ failmsg ">=" a b

lteq :: (Monad m) => LoxObject -> LoxObject -> m LoxObject
lteq (Number n) (Number n') = return $ Boolean $ n <= n'
lteq a b = fail $ failmsg "<=" a b

lookupBin :: (Monad m) => Token -> m (LoxObject -> LoxObject -> m LoxObject)
lookupBin PLUS = return add
lookupBin MINUS = return sub
lookupBin STAR = return mul
lookupBin SLASH = return divide
lookupBin EQUAL_EQUAL = return eq
lookupBin BANG_EQUAL = return neq
lookupBin GREATER = return gr
lookupBin LESS = return lt
lookupBin GREATER_EQUAL = return greq
lookupBin LESS_EQUAL = return lteq
lookupBin _ = fail "not implemented"

loxNot :: (Monad m) => LoxObject -> m LoxObject
loxNot x = return $ Boolean $ not $ toBool $ truthiness x

neg :: (Monad m) => LoxObject -> m LoxObject
neg (Number n) = return $ Number $ negate n
neg _ = fail "Operation '-' (unary) is only supported for numbers"

lookupUn :: (Monad m) => Token -> m (LoxObject -> m LoxObject)
lookupUn BANG = return loxNot
lookupUn MINUS = return neg
lookupUn _ = fail "not implemented"
