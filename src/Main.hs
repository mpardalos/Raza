module Main where

import Parser

e :: Expr
e = Addition (Str "test") (Not $ Identifier "bool")

s :: Stmt
s = Let ("testVar") (Str "test")

b :: Block
b = Block [Let "wee" (Str "1"), ExprStmt $ Call (Identifier "f") [Str "arg"]]

main :: IO ()
main = do
    putStrLn $ show $ Block [s, ExprStmt $ FunctionDef [] b]

