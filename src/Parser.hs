module Parser where 

import Data.List
import Data.Bifoldable
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

type Name = String

data Block = Block [Stmt]

instance Show Block where
    show (Block stmts) = "{\n    " ++ intercalate ";\n    " (map show stmts) ++ ";\n}"

data Stmt = ExprStmt Expr
          | Let Name Expr
          | Var Name Expr

instance Show Stmt where 
    show (ExprStmt e) = "ExprStmt(" ++ show e                      ++ ")"
    show (Var name e) = "Var("      ++ show name ++ ", " ++ show e ++ ")"
    show (Let name e) = "Let("      ++ show name ++ ", " ++ show e ++ ")"

data BinOp = Add | Sub | Mul | Div | Equal | NotEqual | Greater | Less | LessEqual | GreaterEqual
           deriving Show

data UnaryOp = Not | Minus deriving Show

data Expr = Binary BinOp Expr Expr
          | Unary UnaryOp Expr

          | Str String
          | Numeric (Either Integer Double)
          | Identifier Name

          | True
          | False
          | Nil

          | If Expr Block Block

          | FunctionDef [Name] Block
          | Call Expr [Expr]


instance Show Expr where
    show (Binary op l r)      = show op ++ "(" ++ show l ++ ", " ++ show r ++ ")"
    show (Unary op e)         = show op ++ "(" ++ show e ++ ")"

    show (Call c as)          = "call("        ++ show c ++ ", " ++ show as ++ ")"

    show (Str s)              = valueShow "string" (show s)
    show (Numeric n)          = valueShow "num" (bifoldMap show show n)
    show (Identifier i)       = valueShow "identifier" (show i)
    show Parser.True          = "True"
    show Parser.False         = "False"
    show Nil                  = "Nil"
    show (If cond _ _)        = "if(" ++ show cond ++ ", ... )"
    show (FunctionDef args b) = "fun(" ++ show args ++ ", " ++ show b ++ ")"

valueShow :: String -> String -> String
valueShow name val = name ++ "[" ++ val ++ "]"

-- Lexer
tokenDef :: LanguageDef st
tokenDef = emptyDef { commentStart = "/*"
                    , commentEnd = "*/"
                    , commentLine = "//"
                    , identStart = letter <|> oneOf "_$" <?> "identifier"
                    , identLetter = alphaNum <|> oneOf "_$"
                    , reservedNames = ["fun", "var", "let", "or", "and" , "if", "else"
                                      , "while", "return", "True" , "False" , "Nil"
                                      ]
                    , reservedOpNames = [ "||", "&&", "+", "-", "/", "*", "=>"
                                        , "!=", "==", "<", ">", "<=", ">="
                                        , "!" 
                                        ]
                    }

lexer         = Token.makeTokenParser tokenDef
identifier    = Token.identifier lexer
reserved      = Token.reserved lexer
reservedOp    = Token.reservedOp lexer
stringLiteral = Token.stringLiteral lexer
numLiteral    = Token.naturalOrFloat lexer
parens        = Token.parens lexer
braces        = Token.braces lexer
brackets      = Token.brackets lexer
commaSep      = Token.commaSep lexer
symbol        = Token.symbol lexer

-- Parser

operators :: OperatorTable Char st Expr
operators = [[ Prefix (reserved "-" >> return (Unary Minus))
             , Prefix (reserved "!" >> return (Unary Not))
             ] 
            ,[ Infix (reservedOp "==" >> return (Binary Equal)) AssocLeft
             , Infix (reservedOp "!=" >> return (Binary NotEqual)) AssocLeft
             , Infix (reservedOp "<" >> return (Binary Less)) AssocLeft
             , Infix (reservedOp ">" >> return (Binary Greater)) AssocLeft
             , Infix (reservedOp "<=" >> return (Binary LessEqual)) AssocLeft
             , Infix (reservedOp ">=" >> return (Binary GreaterEqual)) AssocLeft
             ]
            ,[ Infix (reservedOp "*" >> return (Binary Mul)) AssocLeft
             , Infix (reservedOp "/" >> return (Binary Div)) AssocLeft
             , Infix (reservedOp "+" >> return (Binary Add)) AssocLeft
             , Infix (reservedOp "-" >> return (Binary Sub)) AssocLeft
             ]
            ]

term :: GenParser Char st Expr
term = parens expression
    <|> (reserved "False" >>= \_ -> return Parser.False)
    <|> (reserved "True" >> return Parser.True)
    <|> (reserved "Nil" >> return Parser.True)
    <|> try ifExpr
    <|> try funExpr
    <|> liftM Str stringLiteral
    <|> liftM Numeric numLiteral
    <|> liftM Identifier identifier
    <?> "primary expression"

expression :: GenParser Char st Expr
expression = try call 
    <|> buildExpressionParser operators term 
    <?> "expression"

-- ! Issue: statements must be on different lines or have semicolon seperators. 
--          currently, the lexer consumes newlines so they cannot be parsed as statement 
--          seperators, so they have been made optional
stmt :: GenParser Char st Stmt
stmt = do { s <- (letStmt <|> varStmt <|> exprStmt) 
          ; _ <- optional stmtEnd
          ; return s
          }
    <?> "statement"

block :: GenParser Char st Block
block = (braces (many stmt) >>= return . Block)
    <|> (expression >>= \e -> return $ Block [ExprStmt e])
    <?> "block"

letStmt :: GenParser Char st Stmt
letStmt = do { _    <- reserved "let"
             ; name <- identifier
             ; _    <- symbol "="
             ; val  <- expression
             ; return $ Let name val
             }
             <?> "let statement"

varStmt :: GenParser Char st Stmt
varStmt = do { _    <- reserved "var"
             ; name <- identifier
             ; _    <- symbol "="
             ; val  <- expression
             ; return $ Var name val
             }
             <?> "var statement"

exprStmt :: GenParser Char st Stmt
exprStmt = expression >>= (return . ExprStmt)

ifExpr :: GenParser Char st Expr
ifExpr = do { _         <- reserved "if"
            ; cond      <- expression
            ; ifBlock   <- block 
            ; _         <- reserved "else"
            ; elseBlock <- try block
            ; return $ If cond ifBlock elseBlock
            }
            <?> "if"

funExpr :: GenParser Char st Expr
funExpr = do { _        <- reserved "fun"
             ; argNames <- parens $ commaSep identifier
             ; _        <- symbol "=>"
             ; body     <- block
             ; return $ FunctionDef argNames body
             }
             <?> "function expression"

call :: GenParser Char st Expr
call = do { callee <- term
          ; args   <- parens $ commaSep expression
          ; return $ Call callee args
          }
          <?> "call"


stmtEnd :: GenParser Char st ()
stmtEnd = void (symbol ";") <|> (lookAhead $ void (symbol "}")) <|> void eof
        <?> "statement terminator"

parseString :: String -> Either ParseError Block
parseString input = parse (many stmt >>= (return . Block)) "" input


