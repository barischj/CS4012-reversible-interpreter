module Lib where
import qualified Data.Map               as Map

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State hiding (state)

-- The pure expression language.

data Val =
      I Int
    | B Bool
    deriving (Eq, Read, Show)

data Expr =
      Const Val
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | And Expr Expr
    | Or  Expr Expr
    | Not Expr
    | Eq  Expr Expr
    | Gt  Expr Expr
    | Lt  Expr Expr
    | Var Name
    deriving (Eq, Read, Show)

type Name = String
type Env  = Map.Map Name Val

lookupF :: Monad m => Name -> Map.Map Name a -> m a
lookupF k t =
    case Map.lookup k t of
        Just x  -> return x
        Nothing -> fail ("Unknown variable "++k)

-- Monadic style expression evaluator.
type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity $ runExceptT $ runReaderT ex env

-- Integer typed expressions.
evali op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ I (i0 `op` i1)
        _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions.
evalb op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (B i0, B i1) -> return $ B (i0 `op` i1)
        _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans.
evalib op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ B (i0 `op` i1)
        _            -> fail "type error in arithmetic expression"

-- Evaluate an expression.
eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1

eval (And e0 e1) = evalb (&&) e0 e1
eval (Or  e0 e1) = evalb (||) e0 e1

eval (Not e0   ) = evalb (const not) e0 (Const (B True))

eval (Eq e0 e1) = evalib (==) e0 e1
eval (Gt e0 e1) = evalib (>) e0 e1
eval (Lt e0 e1) = evalib (<) e0 e1

eval (Var s) = do
    env <- ask
    lookupF s env

-- The statement language.
data Statement =
      Assign Name Expr
    | If Expr Statement Statement
    | While Expr Statement
    | Print Expr
    | Seq Statement Statement
    | Try Statement Statement
    | Pass
    deriving (Eq, Read, Show)

-- History consists of all previous statements and maybe value of a variable
-- prior to assignment.
type HistoryItem = (Statement, Maybe (Name, Val))
type History = [HistoryItem]

-- All statements remaining to be evaluated.
type Future = [Statement]

-- State in the SEval monad consists of the history of previous statements, the
-- current evaluation environment and the statements remaining to be evaluated.
data IState = IState {
        iSHist :: History,
        iSEnv  :: Env
        -- iSFuture :: Future
    } deriving Show

newIState :: IState
newIState = IState { iSHist = [], iSEnv = Map.empty }

-- Get and set state environment.
getEnv :: SEval Env
getEnv = iSEnv <$> get

setEnv :: Env -> SEval ()
setEnv env = modify (\s -> s { iSEnv = env })

-- Save a statement and possible assignment to history.
save :: Statement -> Maybe Name -> SEval ()
save statement maybeName = do
    history <- getHistory
    case maybeName of
        Nothing   -> saveNothing history
        Just name -> do
            env <- getEnv
            case Map.lookup name env of
                Nothing  -> saveNothing history
                Just val -> saveVal history name val

    where saveNothing hist = setHistory $ hist ++ [(statement, Nothing)]
          saveVal hist name val =
              setHistory $ hist ++ [(statement, Just (name, val))]

-- Get and set state history.
getHistory :: SEval History
getHistory = iSHist <$> get

setHistory :: History -> SEval ()
setHistory history = modify (\s -> s { iSHist = history })

-- Print interpreter output.
putInfo :: String -> SEval ()
putInfo str = liftIO $ putStrLn $ "> " ++ str

-- Print and throw error.
throw :: String -> SEval a
throw err = do
    putInfo $ "ERR: " ++ err
    throwError err

-- Monadic style statement evaluator.
type SEval a = StateT IState (ExceptT String IO) a

-- Run the SEval monad where state contains the given statements.
runSEval :: SEval a -> IO (Either String (a, IState))
runSEval sEvalA = runExceptT $ runStateT sEvalA newIState

-- Evaluate an expression in the SEval monad.

sExpr :: Expr -> SEval Val
sExpr expr = do
    env <- getEnv
    case runEval env (eval expr) of
        Left  err -> throw err
        Right val -> return val

sExprB :: Expr -> SEval Bool
sExprB expr = do
    val <- sExpr expr
    case val of
        B bool -> return bool
        a      -> throw $ "Expected B Bool, got " ++ show a

-- Statement handlers for the interpreter -------------------------------------

sEval :: Statement -> SEval ()

sEval stmt@(Assign name expr) = do
    save stmt $ Just name
    env <- getEnv
    val <- sExpr expr
    setEnv $ Map.insert name val env
    putInfo $ concat ["Assigned ", show val, " to ", show name]

sEval (If expr sTrue sFalse) = do
    val <- sExprB expr
    putInfo $ "If guard " ++ show val
    if   val
    then prompt sTrue
    else prompt sFalse

sEval while@(While expr statement) = do
    val <- sExprB expr
    putInfo $ "While guard " ++ show val
    when val $ do
      prompt statement
      putInfo "While iteration finished"
      prompt while

sEval (Print expr) = liftIO $ putStrLn $ "Print: " ++ show expr

sEval stmt@(Seq s1 s2) = do
    save stmt Nothing
    putInfo "running Seq"
    prompt s1
    prompt s2

sEval (Try sTry sCatch) = do
    putInfo "running Try"
    prompt sTry `catchError` handler
    where handler _ = do
            putInfo "caught error"
            prompt sCatch

sEval Pass = putInfo "Pass"

-- Interactive prompt for a statement.
prompt :: Statement -> SEval ()
prompt statement = do
    putInfo $ "Next statement: " ++ safeTake (show statement)
    putInfo "i (inspect) / c (continue) / b (back) / q (quit)"
    input <- liftIO getLine
    case input of
        "b" -> do -- print current state and see what we can do
               putInfo "current state"
               state <- get
               putInfo $ show state
               prompt statement
               -- The statement we are currently evaluating is the last in our
               -- state history. So we take the second last statement which is
               -- semantically the previously evaluated statement. We then begin
               -- evaluating that statement.
        "c" -> sEval statement
        "i" -> inspectPrompt       >> prompt statement
        "q" -> fail "quitting..."
        _   -> putInfo "bad input" >> prompt statement

-- Run the prompt on a statament, catching any errors.
runInterpreter :: Statement -> IO ()
runInterpreter statement = void $ runSEval catchRoot
    where catchRoot =
            sEval statement `catchError`
                 const (putInfo "Uncaught error")

-- Inspection functions -------------------------------------------------------

-- Interactive prompt to inspect the history of variables.
inspectPrompt :: SEval ()
inspectPrompt = do
    putInfo "i X (inspect X) / e (current environement) / q (quit inspection)"
    input <- liftIO getLine
    case input of
        ['i', ' ', name] -> printVarHistory [name] >> inspectPrompt
        "q"              -> return ()
        "e"              -> printEnv               >> inspectPrompt
        _                -> putInfo "bad input"    >> inspectPrompt

-- Prints the history of a variable and its current value.
printVarHistory :: Name -> SEval ()
printVarHistory name = do
    history <- getHistory
    mapM_ (printHistoryItemIfName name) history
    printCurrentVar name

-- Prints a statement and value of a variable prior to its execution IF that
-- variable is of given name.
printHistoryItemIfName :: Name -> HistoryItem -> SEval ()
printHistoryItemIfName name (_, maybeVar) =
    case maybeVar of
        Nothing           -> return ()
        Just (name', val) ->
            when (name == name') $ putInfo $ concat [name, " = ", show val]

-- Prints the current value of a variable.
printCurrentVar :: Name -> SEval ()
printCurrentVar name = do
    env <- getEnv
    case Map.lookup name env of
        Nothing  -> putInfo $ name ++ " is undefined"
        Just val -> putInfo $ concat [name, " = ", show val] 

-- Prints the current environment.
printEnv :: SEval ()
printEnv = do
    env <- getEnv
    printEnv' $ Map.toList env

printEnv' :: [(Name, Val)] -> SEval ()
printEnv' [] = return ()
printEnv' ((name, val):xs) = do
    putInfo $ concat [name, " = ", show val]
    printEnv' xs

-- Take upto n chars of a string if there are enough.
safeTake :: String -> String
safeTake = safeTake' 30

safeTake' :: Int -> String -> String
safeTake' _ [] = []
safeTake' n (x:xs)
    | n > 0     = x : safeTake' (n - 1) xs
    | otherwise = "..."
