module Helpers.Parser where

import Control.Applicative (Alternative(..))
import Helpers.Types
import Debug.Trace

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
  | x == '(' = [x] : tokenize xs
  | x == ')' = [x] : tokenize xs
  -- | x == '\'' = handleQuote xs
  | isNumberCharacter x = tokenizeNumber (x:xs) ""
  | isSymbolCharacter x = tokenizeSymbol (x:xs) ""
  | otherwise = tokenize xs

handleQuote :: String -> [String]
handleQuote xs =
  case xs of
    '(':rest -> "'(" : tokenize rest
    _ -> "'" : tokenize xs

tokenizeNumber :: String -> String -> [String]
tokenizeNumber [] number = [number]
tokenizeNumber (x:xs) number
  | isNumberCharacter x = tokenizeNumber xs (number ++ [x])
  | otherwise = number : tokenize (x:xs)

tokenizeSymbol :: String -> String -> [String]
tokenizeSymbol [] symbol = [symbol]
tokenizeSymbol (x:xs) number
  | isSymbolCharacter x = tokenizeSymbol xs (number ++ [x])
  | otherwise = number : tokenize (x:xs)

-- Тип Parser
newtype Parser a = Parser { runParser :: [String] -> Either String ([String], a) }

-- Инстансы Functor, Applicative, Monad, Alternative
instance Functor Parser where
  fmap f (Parser p) = Parser $ \tokens -> case p tokens of
    Left err -> Left err
    Right (rest, a) -> Right (rest, f a)

instance Applicative Parser where
  pure a = Parser $ \tokens -> Right (tokens, a)
  Parser pf <*> Parser pa = Parser $ \tokens -> case pf tokens of
    Left e1 -> Left e1
    Right (rest, f) -> case pa rest of
      Left e2 -> Left e2
      Right (rest2, a) -> Right (rest2, f a)

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \tokens -> case p tokens of
    Left err -> Left err
    Right (rest, a) -> runParser (f a) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Empty"
  Parser pa <|> Parser pb = Parser $ \tokens -> case pa tokens of
    Left _ -> pb tokens
    Right (rest, a) -> Right (rest, a)



-- Парсер для SExpr
parseSExpr :: Parser SExpr
parseSExpr = -- trace "Entering parseSExpr" $
  parseDefine <|> parseSetVar <|> parseNumber <|> parseLambda <|> parseIf <|> parseQuote <|> parseQuoteSugar
  <|> parseCompareOp <|> parseList <|> parseArithOp 
  <|> parseBool <|> parseAnd <|> parseOr <|> parseNot
  <|> parseStringCheck <|> parseStringLength 
  <|> parseStringEqual <|> parseSubstring <|> parseStringAppend
  <|> parseStringLiteral
  <|> parseCadrLike 
  <|> parseAtom

parseQuote :: Parser SExpr
parseQuote = do
  token "quote"
  expr <- parseSExpr
  return $ Quote expr

parseQuoteSugar :: Parser SExpr
parseQuoteSugar = do
  token "'"
  expr <- parseSExpr
  -- trace ("[PARSER] Quote Sugar entered with: " ++ show expr) $ pure ()
  return $ Quote expr

-- Базовые парсеры
token :: String -> Parser String
token expected = Parser $ \tokens -> 
  case tokens of
  (t:ts) | t == expected -> Right (ts, t)
  _ -> Left $ "Expected token: " ++ expected

atom :: Parser String
atom = Parser $ \tokens -> case tokens of
  (t:ts) -> if all isSymbolCharacter t
              then Right (ts, t)    
              else Left $ "Expected a symbol, but got: " ++ t
  _ -> Left "Expected an atom"

number :: Parser SExpr
number = Parser $ \tokens -> case tokens of
  (t:ts) -> case reads t of
    [(n, "")] -> Right (ts, Number n)
    _ -> Left $ "Expected a number, but got: " ++ t
  _ -> Left "Expected a number"

-- Парсер для define
parseDefine :: Parser SExpr
parseDefine = do
  token "define"
  var <- atom
  value <- parseSExpr
  return $ Define var value

-- Парсер для работы с переменными
parseSetVar :: Parser SExpr
parseSetVar = do
  token "set!"
  var <- atom
  value <- parseSExpr
  return $ Var var value

parseStringLiteral :: Parser SExpr
parseStringLiteral = do
  t <- Parser $ \tokens -> case tokens of
    (t:ts) -> Right (ts, t) 
    _ -> Left "Expected a string literal"

  if not (null t) && head t == '\"' && last t == '\"'
    then do
      let content = init (tail t)
      let unescapedContent = unescapeString content
      return $ StringLiteral unescapedContent
    else
      empty 

unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\\':'"':xs) = '"' : unescapeString xs
unescapeString ('\\':'\\':xs) = '\\' : unescapeString xs
unescapeString (x:xs) = x : unescapeString xs 

parseStringCheck :: Parser SExpr
parseStringCheck = do
  token "string?"
  trace "string check parsed" $ pure()
  arg <- parseSExpr
  return $ StringCheck arg

parseStringLength :: Parser SExpr
parseStringLength = do
  token "string-length"
  arg <- parseSExpr
  return $ StringLength arg

parseStringEqual :: Parser SExpr
parseStringEqual = do
  token "string=?"
  -- trace "[PARSER] parsed STRING EQUAL" $ pure()
  arg1 <- parseSExpr
  arg2 <- parseSExpr
  return $ StringEqual arg1 arg2

parseSubstring :: Parser SExpr
parseSubstring = do
  token "substring"
  str <- parseSExpr
  start <- parseSExpr
  end <- parseSExpr
  return $ Substring str start end

parseStringAppend :: Parser SExpr
parseStringAppend = do
  token "string-append"
  -- trace "string-append parsing args" $ pure()
  args <- many parseSExpr 
  trace ("args parsed: " ++ show args) $ pure ()
  return $ StringAppend args

car :: SExpr -> SExpr
car (Cons x _) = x
car _ = error "car: expected a non-empty list"

cdr :: SExpr -> SExpr
cdr (Cons _ xs) = xs
cdr _ = error "cdr: expected a non-empty list"

parseCadrLike :: Parser SExpr
parseCadrLike = do
  funcName <- atom
  if isCadrLike funcName
    then do
      arg <- parseSExpr
      let expr = buildCadrExpr funcName arg
      return expr
    else empty
  where
    isCadrLike :: String -> Bool
    isCadrLike name =
      length name >= 3 &&
      head name == 'c' &&
      last name == 'r' && 
      all (`elem` "ad") (init (tail name))

    buildCadrExpr :: String -> SExpr -> SExpr
    buildCadrExpr name arg =
      foldr (\ch acc -> if ch == 'a' then Car acc else Cdr acc) arg (reverse $ init (tail name))

parseBool :: Parser SExpr
parseBool = do
  val <- atom
  case val of
    "#t" -> return $ Bool True
    "#f" -> return $ Bool False
    _    -> empty

parseAnd :: Parser SExpr
parseAnd = do
  token "and"
  args <- many parseSExpr
  return $ LogicBinary And args

parseOr :: Parser SExpr
parseOr = do
  token "or"
  args <- many parseSExpr 
  return $ LogicBinary Or args

parseNot :: Parser SExpr
parseNot = do
  token "not"
  arg <- parseSExpr
  return $ LogicUnary Not arg

-- Парсер для числа
parseNumber :: Parser SExpr
parseNumber = Parser $ \tokens -> case tokens of
  (t:ts) -> case reads t of
    [(n, "")] -> trace ("Parsed number: " ++ show n) $ Right (ts, Number n)
    _ -> Left $ "Expected a number, but got: " ++ t
  _ -> Left "Expected a number"

-- Парсер для лямбда-выражений
parseLambda :: Parser SExpr
parseLambda = do
  token "lambda"
  -- trace "Parsing lambda" $ pure ()
  params <- parseParams
  -- -- trace ("Parsed params: " ++ show params) $ pure ()
  body <- parseSExpr
  -- trace ("Parsed body: " ++ show body) $ pure ()
  pure (Lambda params body)

-- Парсер для списка параметров
parseParams :: Parser [String]
parseParams = do
  token "("
  -- trace "Parsing params" $ pure ()
  params <- many atom
  -- trace ("Parsed params " ++ show params) $ pure ()
  token ")"
  return params

-- Парсер для атома
parseAtom :: Parser SExpr
parseAtom = Atom <$> atom

parseList :: Parser SExpr
parseList = do
  token "("
  trace "parsing List" $ pure ()
  expr <- parseSExpr
  trace ("List parsed: " ++ show expr) $ pure ()
  rest <- parseListTail
  return $ case rest of
    Nil -> expr
    _ -> Cons expr rest 

-- Парсер для хвоста списка
parseListTail :: Parser SExpr
parseListTail = do
  (token ")" *> pure Nil) <|> do
    hd <- parseSExpr
    tl <- parseListTail
    pure $ Cons hd tl

-- Парсер для арифметических операций
parseArithOp :: Parser SExpr
parseArithOp = do
  op <- atom
  if op `elem` ["+", "-", "*", "/"]
    then do
      -- trace "Parsing arith op" $ pure ()
      args <- many parseSExpr
      trace ("Parsed arithmetic operation: op = " ++ show op ++ ", args = " ++ show args) $ pure (ArithOp (head op) args)
    else empty

parseCompareOp :: Parser SExpr
parseCompareOp = do
  op <- atom
  if op `elem` ["<", ">", "<=", ">=", "==", "!="]
    then do
      args <- many parseSExpr
      return $ CompareOp op args
    else empty

parseIf :: Parser SExpr
parseIf = do
  token "if"
  -- trace "Parsing if" $ pure ()
  condition <- parseSExpr
  -- trace ("Parsed condition: " ++ show condition ) $ pure ()
  thenBranch <- parseSExpr
  elseBranch <- parseSExpr
  return $ If condition thenBranch elseBranch

-- Пример использования
main :: IO ()
main = do
  -- let input = ["if", "(", "<", "1", "2", ")", "aaa", "bbb"]
  -- let input = ["lambda", "(", "x", "y", ")", "(", "+", "x", "y", ")"]
  -- let input = ["(", "and" , "#f", "#t", ")"]
  -- let input = ["or", "#f", "(", "<", "1", "2", ")", "aaa"]
  -- let input = ["'smth"]
  -- let input = ["(", "define", "x", "10", ")"]
  -- let input = ["define", "y", "(", "+", "1", "2", ")"]
  -- let input = ["()", "y", "(", "+", "1", "2", ")"]
  -- let input = ["(", "+", "a", "b", ")"]
  -- let input = ["(", "string-append", "\"hello\"" , "\" world\"", ")"]
  -- let input = ["(", "string-append", "\" world\"", ")"]
  -- let input = ["\" world\""]
  let input = ["()"]
  print input
  case runParser parseSExpr input of
    Left err -> putStrLn $ "Error: " ++ err
    Right (rest, expr) -> do
      print expr
      print rest