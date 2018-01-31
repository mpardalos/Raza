module Parser where 

import Data.List

type Name = String

data Block = Block [Stmt]

instance Show Block where
    show (Block stmts) = "{\n  " ++ intercalate ";\n  " (map show stmts) ++ ";\n}"

data Stmt = ExprStmt Expr
          | Let Name Expr
          | Var Name Expr

instance Show Stmt where 
    show (ExprStmt e) = "ExprStmt(" ++ show e ++ ")"
    show (Var name e) = "Var(" ++ show name ++ ", " ++ show e ++ ")"
    show (Let name e) = "Let(" ++ show name ++ ", " ++ show e ++ ")"

data Expr = Addition Expr Expr
          | Subtraction Expr Expr
          | Multiplication Expr Expr

          | Not Expr
          | Minus Expr
          | Call Expr [Expr]

          | Str String
          | forall a . (Num a, Show a) => Numeric a
          | Identifier Name

          | True
          | False
          | Nil
          | If Expr Block Block
          | FunctionDef [Name] Block

binaryShow :: (Show a, Show b) => String -> a -> b -> String
binaryShow name l r = name ++ "(" ++ show l ++ ", " ++ show r ++ ")"

unaryShow :: Show a => String -> a -> String
unaryShow name e = name ++ "(" ++ show e ++ ")"

valueShow :: Show a => String -> a -> String
valueShow name val = name ++ "[" ++ show val ++ "]"

instance Show Expr where
    show (Addition l r)       = binaryShow "add" l r
    show (Subtraction l r)    = binaryShow "minus" l r
    show (Multiplication l r) = binaryShow "times" l r

    show (Not e)              = unaryShow "not" e
    show (Minus e)            = unaryShow "minus" e
    show (Call callee args)   = binaryShow "call" callee args

    show (Str s)              = valueShow "string" s
    show (Numeric n)          = valueShow "num" n
    show (Identifier n)       = valueShow "identifier" n
    show Parser.True          = "True"
    show Parser.False         = "False"
    show Nil                  = "Nil"
    show (If cond _ _)        = "if(" ++ show cond ++ ", ... )"
    show (FunctionDef args b) = "fun(" ++ show args ++ ", " ++ show b ++ ")"


