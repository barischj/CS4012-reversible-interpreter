module Lib where

import qualified Data.Map as Map
import qualified Safe
  
import qualified Control.Monad as Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

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
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or  e0 e1) = do evalb (||) e0 e1

eval (Not e0   ) = do
    evalb (const not) e0 (Const (B True))

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

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

-- All previous statements and maybe value of a variable prior to assignment.
type History = [(Env, Maybe (Name, Val))]

-- All statements remaining to be evaluated.
type Future = [Statement]

-- State in the SEval monad consists of the history of previous statements, the
-- current evaluation environment and the statements remaining to be evaluated.
data IState = IState {
    -- iSHist   :: History,
    iSEnv    :: Env
    -- iSFuture :: Future
  }

newIState :: IState
newIState = IState { iSEnv = Map.empty }

-- Utility functions to get and set state.
getEnv :: SEval Env
getEnv = iSEnv <$> get

setEnv :: Env -> SEval ()
setEnv env = modify (\state -> state { iSEnv = env })
  
-- Monadic style statement evaluator.
type SEval a = StateT IState (ExceptT String IO) a

-- Run the SEval monad where state contains the given statements.
runSEval :: SEval a -> IO (Either String (a, IState))
runSEval sEvalA  = runExceptT $ runStateT sEvalA newIState

-- Evaluate an expression in the SEval monad.

sExpr :: Expr -> SEval Val
sExpr expr = do
    env <- getEnv
    case runEval env (eval expr) of
        Left  err -> fail err
        Right val -> return val

sExprB :: Expr -> SEval Bool
sExprB expr = do
    val <- sExpr expr
    case val of
        B bool -> return bool
        _      -> fail "type error in expression"
        
-- Evaluate a statement in the SEval monad.

sEval :: Statement -> SEval ()

sEval (Assign name expr) = do
    env <- getEnv
    val <- sExpr expr
    setEnv $ Map.insert name val env 
    liftIO $ putStrLn $ concat ["Assigned ", show val, " to ", show name]

sEval (If expr strue sfalse) = do
    val <- sExprB expr
    case val of
        False -> do
            liftIO $ putStrLn "if guard false"
            prompt sfalse
        True  -> do
            liftIO $ putStrLn "if guard true"
            prompt strue

sEval while@(While expr statement) = do
    val <- sExprB expr
    case val of
        False ->
            liftIO $ putStrLn "while guard false"
        True  -> do
            liftIO $ putStrLn "while guard true"
            prompt statement
            liftIO $ putStrLn "while iteration finished"
            prompt while

-- sEval (Print expr)  

sEval (Seq s1 s2) = do
    liftIO $ putStrLn "running Seq"
    prompt s1
    prompt s2

-- Interpreter prompt within the SEval monad.
prompt :: Statement -> SEval ()
prompt statement = do
    liftIO $ putStrLn "i (inspect) / c (continue) / b (back) / q (quit)"
    input <- liftIO $ getLine
    env <- getEnv
    case input of
        "i" -> liftIO $ putStrLn$ show env
        "c" -> sEval statement
        "q" -> fail "quitting..."

runInterpreter :: Statement -> IO ()
runInterpreter statement = void $ runSEval (sEval statement)
