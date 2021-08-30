module MathParser 
    ( Expr (..)
    , Func (..)
    , eval
    , eval'
    , parseExpression
    ) where

import Text.Parsec
import Text.Parsec.String

data Expr 
    = Number Double
    | Symbol String
    | Oper1 Func Expr
    | Oper2 Func Expr Expr
    | Func1 Func Expr
    | Func2 Func Expr Expr
    deriving (Show, Eq)

data Func 
    = UnaryPlus    -- ^ op      1 argument   '+'
    | UnaryMinus   -- ^ op      1 argument   '-'
    | Plus         -- ^ op      2 argument   '+'
    | Minus        -- ^ op      2 argument   '-'
    | Multip       -- ^ op      2 argument   '*'
    | Divide       -- ^ op      2 argument   '/'
    | Power        -- ^ op      2 argument   '^'
    | LogBase      -- ^ func    2 argument   'logBase'
    | Log          -- ^ func    1 argument   'log'
    | Sin          -- ^ func    1 argument   'sin'
    | Cos          -- ^ func    1 argument   'cos'
    | Tan          -- ^ func    1 argument   'tan'
    | Cot          -- ^ func    1 argument   'cot'
    | Exp          -- ^ func    1 argument   'exp'
    deriving (Show, Eq)


{- Evaluating -}

eval :: String -> Double -> Double
eval s a = eval' (parseExpression s) a

eval' :: Expr -> Double -> Double
eval' (Number x) _             = x
eval' (Symbol _) a             = a
eval' (Oper1 UnaryPlus  x  ) a = eval' x a
eval' (Oper1 UnaryMinus x  ) a = negate $ eval' x a
eval' (Oper1 _          _  ) _ = error "undefined operator"
eval' (Oper2 Plus       x y) a = eval' x a +  eval' y a
eval' (Oper2 Minus      x y) a = eval' x a -  eval' y a
eval' (Oper2 Multip     x y) a = eval' x a *  eval' y a
eval' (Oper2 Divide     x y) a = eval' x a /  eval' y a
eval' (Oper2 Power      x y) a = eval' x a ** eval' y a
eval' (Oper2 _          _ _) _ = error "undefined operator"
eval' (Func1 Log        x  ) a = log $ eval' x a
eval' (Func1 Sin        x  ) a = sin $ eval' x a
eval' (Func1 Cos        x  ) a = cos $ eval' x a
eval' (Func1 Tan        x  ) a = tan $ eval' x a
eval' (Func1 Cot        x  ) a = (1/) . tan $ eval' x a
eval' (Func1 Exp        x  ) a = exp $ eval' x a
eval' (Func1 _          _  ) _ = error "undefined function"
eval' (Func2 LogBase    x y) a = logBase (eval' x a) (eval' y a)
eval' (Func2 _          _ _) _ = error "undefined function"


{- Parser -}

parseExpression :: String -> Expr
parseExpression s =   
    case parse (whitespace *> parseExpression' <* eof) "" s of
        Left e  -> error $ show e
        Right x -> x

parseExpression' :: Parser Expr 
parseExpression' 
    =  priority2
   <|> priority1
   <|> unary
   <|> priority0
   <|> roundBrace parseExpression'
   <|> parseFunction1
   <|> parseFunction2 
   <|> number
   <|> symbol

unary :: Parser Expr
unary = try $ do 
    operator <- choice
        [ UnaryPlus  <$ char '+'
        , UnaryMinus <$ char '-'
        ]
    whitespace
    expression <- parseExpression''
    return $ Oper1 operator expression
  where
    parseExpression'' 
        =  roundBrace parseExpression'
       <|> parseFunction1
       <|> parseFunction2 
       <|> number
       <|> symbol
    
priority0 :: Parser Expr
priority0 = try $ parseExpression'' `chainl1` operator 
  where
    operator = choice
        [ Oper2 Power   <$ char '^'
        ] <* whitespace

    parseExpression'' 
        =  roundBrace parseExpression'
       <|> parseFunction1
       <|> parseFunction2 
       <|> number
       <|> symbol

priority1 :: Parser Expr
priority1 = try $ parseExpression'' `chainl1` operator
  where
    operator = choice
        [ Oper2 Multip  <$ char '*'
        , Oper2 Divide  <$ char '/'
        ] <* whitespace

    parseExpression'' 
        =  priority0
       <|> roundBrace parseExpression'
       <|> parseFunction1
       <|> parseFunction2 
       <|> number
       <|> symbol

priority2 :: Parser Expr
priority2 = try $ parseExpression'' `chainl1` operator
  where
    operator = choice 
        [ Oper2 Plus  <$ char '+'
        , Oper2 Minus <$ char '-'
        ] <* whitespace
     
    parseExpression''
        =  priority1
       <|> priority0
       <|> roundBrace parseExpression'
       <|> parseFunction1
       <|> parseFunction2 
       <|> number
       <|> symbol


parseFunction1 :: Parser Expr
parseFunction1 = try $ do
    function <- choice 
        [ Log <$ try (string "log")
        , Sin <$ try (string "sin")
        , Cos <$ try (string "cos")
        , Tan <$ try (string "tan")
        , Cot <$ try (string "cot")
        , Exp <$ try (string "exp")
        ]
    whitespace
    expression <- roundBrace parseExpression'
    return $ Func1 function expression

parseFunction2 :: Parser Expr
parseFunction2 = try $ do
    function <- choice
        [ LogBase <$ try (string "logBase")
        ]
    whitespace
    expression1 <- firstArg  parseExpression'
    expression2 <- secondArg parseExpression'
    return $ Func2 function expression1 expression2

number :: Parser Expr
number = do
    number1 <- many1 digit
    number2 <- option "" afterDot
    whitespace
    return $ Number (read $ number1 ++ number2)
  where
    afterDot = do
        dot     <- char '.'
        number' <- many1 digit
        return $ dot:number'

symbol :: Parser Expr
symbol = do
    x <- char 'x'
    whitespace
    return $ Symbol [x]

-- | gives an expression which is a parse first argument [ '(' <fst> ',' ]
firstArg :: Parser a -> Parser a
firstArg = between leftParen comma

-- | gives an expression which is a second argument [ ',' <snd> ')' ]
secondArg :: Parser a -> Parser a
secondArg = between comma rightParen

-- | parse an expression which is an expression between roundBrace
roundBrace :: Parser a -> Parser a
roundBrace = between leftParen rightParen

-- | gives a comma
comma :: Parser Char
comma = char ',' <* whitespace

-- | gives a left paren
leftParen :: Parser Char
leftParen = char '(' <* whitespace

-- | gives a right paren
rightParen :: Parser Char
rightParen = char ')' <* whitespace

-- | skips a white space 
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\t"
















