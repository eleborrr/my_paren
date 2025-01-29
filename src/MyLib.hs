module MyLib where
import Debug.Trace (trace)
import Control.Monad
import Helpers.Parser
import Helpers.Types
import Data.Map as M

data Env = Env
        {
            globals :: M.Map String SExpr
        } deriving Show

env :: Env
env = Env { globals = M.fromList [("x", Number 10), ("y", Number 20)] }

---- Функция eval
eval :: Env -> SExpr -> (SExpr, Env)
eval _ (Number n) = trace ("Evaluating Number: " ++ show n) $ (Number n, env)

eval _ (StringLiteral s) = trace ("Evaluating StringLiteral: " ++ show s) $ (StringLiteral s, env)

eval _ (Bool b) = trace ("Evaluating Bool: " ++ show b) $ (Bool b, env)

eval _ Nil = trace "Evaluating Nil" (Nil, env)

eval env (Atom var) = trace ("Evaluating Atom: " ++ var) $
    case M.lookup var (globals env) of
        Just val -> trace ("Found value for " ++ var ++ ": " ++ show val) (val, env)
        Nothing  -> error $ "Variable not found: " ++ var

eval env (Cons car cdr) = trace "Evaluating Cons" $
    let (carResult, env1) = eval env car
        (cdrResult, env2) = eval env1 cdr
    in (Cons carResult cdrResult, env2)

eval env (ArithOp op args) = trace ("Evaluating ArithOp: " ++ [op]) $
    let evalArgs = Prelude.map (eval env) args
        _ = trace ("Evaluated arguments: " ++ show evalArgs) ()
        nums = Prelude.map toNumber evalArgs
        _ = trace ("Converted to numbers: " ++ show nums) ()
        result = Number $ applyArithOp op nums
        _ = trace ("Result of arithmetic operation: " ++ show result) ()
    in (result, env)
  where
    toNumber (Number n, env1) = n
    toNumber expr = error $ "Comparison operations expect numbers, but got: " ++ show expr

    applyArithOp '+' = sum
    applyArithOp '-' = foldl1 (-)
    applyArithOp '*' = product
    applyArithOp '/' = foldl1 div
    
eval env (CompareOp op args) = trace ("Evaluating CompareOp: " ++ op) $
    let evalArgs = Prelude.map (eval env) args
        _ = trace ("Evaluated arguments: " ++ show evalArgs) ()
        nums = Prelude.map toNumber evalArgs
        _ = trace ("Converted to numbers: " ++ show nums) ()
        result = Bool $ applyCompareOp op nums
        _ = trace ("Result of comparison operation: " ++ show result) ()
    in (result, env)
  where
    toNumber (Number n, env) = n
    toNumber _ = error "Comparison operations expect numbers"

    applyCompareOp "<"  = \[x, y] -> x < y
    applyCompareOp ">"  = \[x, y] -> x > y
    applyCompareOp "<=" = \[x, y] -> x <= y
    applyCompareOp ">=" = \[x, y] -> x >= y
    applyCompareOp "==" = \[x, y] -> x == y
    applyCompareOp "!=" = \[x, y] -> x /= y
    applyCompareOp _    = error "Unsupported comparison operator"

eval env (If cond thenBranch elseBranch) = trace "Evaluating If" $
    case eval env cond of
        (Bool True, env1) -> trace "Condition is true, evaluating thenBranch" $ eval env thenBranch
        (Bool False, env2) -> trace "Condition is false, evaluating elseBranch" $ eval env elseBranch
        _ -> error "Condition must be a boolean"

eval env (LogicBinary op args) = trace ("Evaluating LogicBinary: " ++ show op) $
    let evalArgs = Prelude.map (eval env) args
        _ = trace ("Evaluated arguments: " ++ show evalArgs) ()
        bools = Prelude.map toBool evalArgs
        _ = trace ("Converted to booleans: " ++ show bools) ()
        result = Bool $ applyLogicBinaryOp op bools
        _ = trace ("Result of logic binary operation: " ++ show result) ()
    in (result, env)
  where
    toBool (Bool b, env) = b
    toBool _ = error "Logic operations expect booleans"

    applyLogicBinaryOp And = and
    applyLogicBinaryOp Or  = or

eval env (LogicUnary op arg) = trace ("Evaluating LogicUnary: " ++ show op) $
    case eval env arg of
        (Bool b, newEnv) -> trace ("Evaluated argument: " ++ show b) $ (Bool $ applyLogicUnaryOp op b, env)
        _ -> error "Logic operations expect booleans"
  where
    applyLogicUnaryOp Not = not


eval env (Quote expr) = trace ("Evaluating Quote: " ++ show expr) (expr, env)

eval env (Define var expr) = trace ("Evaluating Define: " ++ var) $
    let (val, env1) = eval env expr
        _ = trace ("Evaluated expression: " ++ show val) ()
        newEnv = env { globals = M.insert var val (globals env1) }
        _ = trace ("Inserted VAL: " ++ show val) ()

    in (val, newEnv) 

eval env (Lambda params body) = trace ("Evaluating Lambda: " ++ show params) $ (Lambda params body, env)

eval _ expr = error $ "Unsupported expression: " ++ show expr

test :: IO ()
test = do
    let initialEnv = env
    let filename = "test.txt"
    content <- readFile filename  
    let linesOfContent = lines content  
    finalEnv <- foldM processLine initialEnv linesOfContent 
    putStrLn $ "Final environment: " ++ show finalEnv

processLine :: Env -> String -> IO Env
processLine env line = do
    let tokens = tokenize line
    case runParser parseSExpr tokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            return env
        Right (rest, expr) -> do
            putStrLn "\n"
            putStrLn $ "Parsed expression: " ++ show expr
            putStrLn "EVALUATING"
            let (result, newEnv) = eval env expr
            putStrLn $ "Result: " ++ show result
            return newEnv