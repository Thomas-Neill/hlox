module Statement where
import Object

data UnOP = Not | Negate
instance Show UnOP where
  show Not = "!"
  show Negate = "-"

data BinOP = Plus | Minus | Mul | Div | Equal | Inequal | Greater | Less | GrEqual | LEqual

instance Show BinOP where
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"
  show Equal = "=="
  show Inequal = "!="
  show Greater = ">"
  show Less = "<"
  show GrEqual = ">="
  show LEqual = "<="

data Expr = Literal LoxObject |
            Unary UnOP Expr |
            Grouping Expr |
            Binary Expr BinOP Expr |
            Variable LValue |
            Assignment LValue Expr |
            InlineIf Expr Expr Expr |
            Funcall Expr [Expr] |
            Rocket String Expr

data LValue = Name String

instance Show LValue where
  show (Name s) = s

instance Show Expr where
  show (Literal l) = show l
  show (Unary t e) = "(" ++ show t ++ " " ++ show e ++ ")"
  show (Grouping e) = "(group " ++ show e ++ ")"
  show (Binary r t l) = "(" ++ show t ++ " " ++ show r ++ " " ++ show l ++ ")"
  show (Variable l) = show l
  show (Assignment l v) = "(set " ++ show l ++ " " ++ show v ++ ")"
  show (InlineIf c i e) = "(if " ++ show c ++ " " ++ show i ++ " " ++ show e ++ ")"
  show (Funcall name others) = show name ++ "(" ++ concat (fmap ((++",") . show) others) ++ ")"

data Statement = Empty |
                Expression Expr |
                Print Expr |
                Declaration LValue Expr |
                Compound [Statement] |
                If Expr Statement Statement |
                While Expr Statement |
                Break

for :: Statement -> Expr -> Statement -> Statement -> Statement
for ini check each body = Compound [ini,While check $ Compound [body,each]]

instance Show Statement where
  show (Expression e) = show e ++ ";"
  show (Print e) = "print " ++ show e ++ ";"
  show (Declaration l e) = "var " ++ show l ++ " = " ++ show e ++ ";"
  show (Compound exprs) = foldl (++) "{" (map show exprs) ++ "}"
  show Empty = ";"
  show (If expr i e) = "if(" ++ show expr ++ ") " ++ show i ++ " else " ++ show e
  show (While expr st) = "while " ++ show expr ++ " " ++ show st
  show Break = "break;"
