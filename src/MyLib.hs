module MyLib where
import Debug.Trace (trace)
import Control.Monad
import Helpers.Parser
import Helpers.Types
import Data.Map as M
import Data.Char (toLower)

data Env = Env
        {
            globals :: M.Map String SExpr
        } deriving (Show, Eq)

initialEnv :: Env
initialEnv = Env { globals = M.fromList [("x", Number 10), ("y", Number 20)] }

---- Функция eval
eval :: Env -> SExpr -> (SExpr, Env)
eval env (Number n) = (Number n, env)
eval env (StringLiteral s) = (StringLiteral s, env)
eval env (Bool b) = (Bool b, env)
eval env Nil = (Nil, env)

eval env (Atom var) = trace ("Evaluating Atom: " ++ var) $
    case M.lookup var (globals env) of
        Just val -> trace ("Found value for " ++ var ++ ": " ++ show val) (val, env)
        Nothing  -> error $ "Variable not found: " ++ var

eval env (Cons car cdr) = trace "[EVAL] Cons" $
    let (carResult, env1) = eval env car
        (cdrResult, env2) = eval env1 cdr
    in (Cons carResult cdrResult, env2)

eval env (ArithOp op args) = 
    let evalArgs = Prelude.map (eval env) args
        nums = Prelude.map toNumber evalArgs
        result = Number $ applyArithOp op nums
    in (result, env)
  where
    toNumber (Number n, _) = n
    toNumber expr = error $ "Comparison operations expect numbers, but got: " ++ show expr

    applyArithOp '+' = sum
    applyArithOp '-' = foldl1 (-)
    applyArithOp '*' = product
    applyArithOp '/' = foldl1 div

eval env (CompareOp op args) = 
    let evalArgs = Prelude.map (eval env) args
        nums = Prelude.map toNumber evalArgs
        result = Bool $ applyCompareOp op nums
    in (result, env)
  where
    toNumber (Number n, _) = n
    toNumber _ = error "Comparison operations expect numbers"

    applyCompareOp "<"  = \[x, y] -> x < y
    applyCompareOp ">"  = \[x, y] -> x > y
    applyCompareOp "<=" = \[x, y] -> x <= y
    applyCompareOp ">=" = \[x, y] -> x >= y
    applyCompareOp "==" = \[x, y] -> x == y
    applyCompareOp "!=" = \[x, y] -> x /= y
    applyCompareOp _    = error "Unsupported comparison operator"

eval env (If cond thenBranch elseBranch) = 
    case eval env cond of
        (Bool True, env1) -> eval env1 thenBranch
        (Bool False, env2) -> eval env2 elseBranch
        _ -> error "Condition must be a boolean"

eval env (LogicBinary op args) = 
    let evalArgs = Prelude.map (eval env) args
        bools = Prelude.map toBool evalArgs
        result = Bool $ applyLogicBinaryOp op bools
    in (result, env)
  where
    toBool (Bool b, _) = b
    toBool _ = error "Logic operations expect booleans"

    applyLogicBinaryOp And = and
    applyLogicBinaryOp Or  = or

eval env (LogicUnary op arg) = 
    case eval env arg of
        (Bool b, newEnv) -> (Bool $ applyLogicUnaryOp op b, newEnv)
        _ -> error "Logic operations expect booleans"
  where
    applyLogicUnaryOp Not = not

eval env (Quote expr) = trace ("[EVAL] Quote got: " ++ show expr) $ (expr, env)

eval env (Define var expr) = 
    let (val, env1) = eval env expr
        newEnv = env1 { globals = M.insert var val (globals env1) }
    in (val, newEnv)

eval env (Var var expr) = 
    let (result, newEnv) = eval env expr
        updatedGlobals = M.insert var result (globals env)
        updatedEnv = newEnv {globals = updatedGlobals}
    in (result, updatedEnv)

eval env (Lambda params body) = (Lambda params body, env)

eval env (StringCheck expr) = 
    case eval env expr of
        (StringLiteral _, env1) -> (Bool True, env1)
        _ -> (Bool False, env)

eval env (StringEqual expr1 expr2) = trace "Evaluating StringEqual" $
    case (eval env expr1, eval env expr2) of
        ((StringLiteral s1, env1), (StringLiteral s2, env2)) -> (Bool (s1 == s2), env2)
        _ -> error "string=? expects two strings"

eval env (Substring strExpr startExpr endExpr) = trace "Evaluating Substring" $
    case (eval env strExpr, eval env startExpr, eval env endExpr) of
        ((StringLiteral s, env1), (Number start, env2), (Number end, env3)) ->
            if start >= 0 && end <= toInteger (length s) && start <= end
                then (StringLiteral (Prelude.take (fromInteger (end - start)) (Prelude.drop (fromInteger start) s)), env3)
                else error "Invalid indices for substring"
        _ -> error "substring expects a string and two numbers"

eval env (StringAppend exprs) = trace "Evaluating StringAppend" $
    let (results, env1) = Prelude.foldl (\(acc, env') expr ->
            case eval env' expr of
                (StringLiteral s, env'') -> (acc ++ [s], env'')
                _ -> error "string-append expects strings"
            ) ([], env) exprs
    in (StringLiteral (concat results), env1)

eval env (Car expr) = 
    case eval env expr of
        (Cons car _, env1) -> (car, env1)
        (atom, env1) -> (atom, env1)

eval env (Cdr expr) = 
    case eval env expr of
        (Cons _ cdr, env1) -> (cdr, env1)
        (_, env1) -> (Nil, env1)

eval env (Atom var) 
    | all (`elem` "ad") (Prelude.map toLower var) = 
        case M.lookup var (globals env) of
            Just val -> trace ("Found value for " ++ var ++ ": " ++ show val) (val, env)
            Nothing  -> error $ "Variable not found: " ++ var
    | otherwise = 
        case M.lookup var (globals env) of
            Just val -> trace ("Found value for " ++ var ++ ": " ++ show val) (val, env)
            Nothing  -> error $ "Variable not found: " ++ var

eval env (Cons (Atom var) expr) 
    | all (`elem` "ad") (Prelude.map toLower var) = 
        let (result, env1) = eval env expr
        in (makeCarCdrFunc var result, env1)
    | otherwise = 
        let (carResult, env1) = eval env (Atom var)
            (cdrResult, env2) = eval env1 expr
        in (Cons carResult cdrResult, env2)

eval _ expr = error $ "Unsupported expression: " ++ show expr

applyCarCdr :: String -> SExpr -> SExpr
applyCarCdr [] expr = expr
applyCarCdr (c:cs) expr = 
    case toLower c of
        'a' -> applyCarCdr cs (Car expr)
        'd' -> applyCarCdr cs (Cdr expr)
        _   -> error $ "Invalid function name: " ++ (c:cs)

makeCarCdrFunc :: String -> SExpr -> SExpr
makeCarCdrFunc name expr = 
    if all (`elem` "ad") (Prelude.map toLower name)
        then applyCarCdr (reverse name) expr
        else error $ "Invalid function name: " ++ name


-- test :: IO ()
-- processLine :: Env -> String -> IO Env
-- processLine curEnv line = do
--     let newEnv = curEnv { globals = M.insert line (Number 2) (globals curEnv) }
--     putStrLn $ "Processing line: " ++ line
--     putStrLn $ "New Env: " ++ show newEnv
--     return newEnv

-- initialEnv = Env { globals = M.empty }

test :: IO ()
test = do
    -- let initialEnv = initialEnv
    let filename = "test.txt"
    content <- readFile filename
    let linesOfContent = lines content
    finalEnv <- foldM processLine initialEnv linesOfContent
    print "end."
    -- putStrLn $ "Final environment: " ++ show finalEnv

processLine :: Env -> String -> IO Env
processLine curEnv line = do
    -- print "CUR ENV:"
    -- print curEnv
    -- print "TOKENS:"
    -- putStrLn ("[PROCESS LINE CUR ENV CHECK]: " ++ show curEnv)
    let tokens = tokenize line
    print tokens
    case runParser parseSExpr tokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            return curEnv
        Right (rest, expr) -> do
            putStrLn "\n"
            -- putStrLn $ "Parsed expression: " ++ show expr
            -- putStrLn "EVALUATING"
            let (result, newEnv) = eval curEnv expr
            putStrLn $ "[PROCESS LINE] Result: " ++ show result
            -- putStrLn $ "[PROCESS LINE] Env: " ++ show newEnv
            return newEnv