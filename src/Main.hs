module Main where

import Parser

e :: Expr
e = Binary Add (Str "test") (Unary Not $ Identifier "bool")

s :: Stmt
s = Let ("testVar") (Str "test")

b :: Block
b = Block [Let "wee" (Str "1"), ExprStmt e] 

main :: IO ()
main = do
    putStrLn $ show $ Block [s, ExprStmt $ FunctionDef [] b]

