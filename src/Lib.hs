module Lib where
import qualified Data.Map               as Map

import Control.Exception
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
eval (Eq  e0 e1) = evalib (==) e0 e1
eval (Gt  e0 e1) = evalib (>) e0 e1
eval (Lt  e0 e1) = evalib (<) e0 e1
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
type History     = [HistoryItem]

-- State in the SEval monad consists of the history of previous statements, the
-- current evaluation environment and the statements remaining to be evaluated.
data IState = IState {
        iSHist :: History,
        iSEnv  :: Env
    } deriving Show

newIState :: IState
newIState = IState { iSHist = [], iSEnv = Map.empty }

-- Get and set state environment.
getEnv :: SEval Env
getEnv = iSEnv <$> get

setEnv :: Env -> SEval ()
setEnv env = modify (\s -> s { iSEnv = env })

modifyEnv :: (Env -> Env) -> SEval ()
modifyEnv f = modify (\s -> s { iSEnv = f (iSEnv s) })

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

data SError = BackError Int | StrError String
  
-- Print and throw error.
throwSErrorStr :: String -> SEval a
throwSErrorStr err = do
    putInfo $ "ERR: " ++ err
    throwError $ StrError err

-- Monadic style statement evaluator.
type SEval a = StateT IState (ExceptT SError IO) a

-- Run the SEval monad where state contains the given statements.
runSEval :: SEval a -> IO (Either SError (a, IState))
runSEval sEvalA = runExceptT $ runStateT sEvalA newIState

-- Evaluate an expression in the SEval monad.

sExpr :: Expr -> SEval Val
sExpr expr = do
    env <- getEnv
    case runEval env (eval expr) of
        Left  err -> throwSErrorStr err
        Right val -> return val

sExprB :: Expr -> SEval Bool
sExprB expr = do
    val <- sExpr expr
    case val of
        B bool -> return bool
        a      -> throwSErrorStr $ "Expected B Bool, got " ++ show a

-- Statement handlers for the interpreter -------------------------------------

-- In case a user has decided to step back through the program, this function
-- catches a step back error, and if we have stepped back enough then
-- evaluation is resumed.
sEval :: Statement -> SEval ()
sEval stmt = sEval' stmt `catchError` handler
    where handler (BackError n)
            | n >  0 = throwError $ BackError (n - 1)
            | n == 0 = do
                putInfo $ "Stepped back to " ++ safeShow stmt
                sEval stmt
          handler e = throwError e 
  
-- This is the function which actually evaluates statements.
sEval' :: Statement -> SEval ()

sEval' stmt@(Assign name expr) = do
    save stmt $ Just name
    env <- getEnv
    val <- sExpr expr
    setEnv $ Map.insert name val env
    putInfo $ concat ["Assigned ", show val, " to ", show name]

sEval' stmt@(If expr sTrue sFalse) = do
    save stmt Nothing
    val <- sExprB expr
    putInfo $ "If guard " ++ show val
    if   val
    then prompt sTrue
    else prompt sFalse

sEval' stmt@(While expr statement) = do
    save stmt Nothing
    val <- sExprB expr
    putInfo $ "While guard " ++ show val
    when val $ do
      prompt statement
      putInfo "While iteration finished"
      prompt stmt

sEval' stmt@(Print expr) = do
    save stmt Nothing
    liftIO $ putStrLn $ "Print: " ++ show expr

sEval' stmt@(Seq s1 s2) = do
    save stmt Nothing
    putInfo "running Seq"
    prompt s1
    prompt s2

sEval' stmt@(Try sTry sCatch) = do
    save stmt Nothing
    putInfo "running Try"
    prompt sTry `catchError` handler
    where handler (StrError err) = do
            putInfo $ "Caught error: " ++ show err
            prompt sCatch
          handler err = throwError err

sEval' Pass = do
    save Pass Nothing
    putInfo "Pass"

-- Interactive prompt for a statement.
prompt :: Statement -> SEval ()
prompt stmt = do
    putInfo $ "Next statement: " ++ safeShow stmt
    putInfo "i (inspect) / c (continue) / b (back) / q (quit)"
    input <- liftIO getLine
    case input of
        "b" -> reverseInterpreter stmt
        "c" -> sEval stmt
        "i" -> inspectPrompt       >> prompt stmt
        "q" -> fail "quitting..."
        _   -> putInfo "bad input" >> prompt stmt

-- | Reverse the interpreter to a previous state if possible.
-- The statement we are currently evaluating is the last in our
-- state history. The statement we want to jump back to is the
-- second last.
reverseInterpreter :: Statement -> SEval()
reverseInterpreter stmt = do
    history <- getHistory
    if   length history < 2
    -- If no previous statement loop the prompt.
    then putInfo "No previous statement" >> prompt stmt
    else reverseState 2 >> throwError (BackError 2)

-- | Reverses the state of the interpreter n steps based on history.
reverseState :: Int -> SEval ()
reverseState 0 = return ()
reverseState rev = do
    history <- getHistory
    case last history of
      (_, Nothing)     -> return ()
      (_, Just (n, v)) -> modifyEnv $ Map.insert n v
    setHistory $ init history
    reverseState $ rev - 1

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

-- Show upto n chars of a showable value.
safeShow :: Show a => a -> String
safeShow = safeTake . show

-- Take upto n chars of a string if there are enough.
safeTake :: String -> String
safeTake = safeTake' 30

safeTake' :: Int -> String -> String
safeTake' _ [] = []
safeTake' n (x:xs)
    | n > 0     = x : safeTake' (n - 1) xs
    | otherwise = "..."
