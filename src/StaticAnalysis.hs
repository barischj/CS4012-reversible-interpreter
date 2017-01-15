module StaticAnalysis (analyse, printErrs) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Set               as Set
import           Expr
import           Interpreter

-- | Log any errors and maintain state.
type Static a = WriterT (Set.Set StaticErr) (StateT StaticState IO) a

-- Run the Static monad.
runStatic :: Static a -> IO ((a, Set.Set StaticErr), StaticState)
runStatic static = runStateT (runWriterT static) emptyState

-- | Looking for uninitialised variable errors and unused variable errors.
data StaticErr = Uninit Name | Unused Name deriving (Eq, Ord)

-- | Track the initialised and accessed variables.
data StaticState = StaticState
    { sInitVars :: Set.Set Name, sAccessVars :: Set.Set Name }

-- | Empty state tracking no variables.
emptyState :: StaticState
emptyState = StaticState Set.empty Set.empty

-- | Find static analysis errors.
analyse :: Statement -> IO [StaticErr]
analyse stmt = do
    ((_, errs), StaticState initialised accessed) <- runStatic $ staticS stmt
    -- Unused variables are those initialised but not accessed.
    let unused = Set.toList $ initialised `Set.difference` accessed
    return $ Set.toList errs ++ map Unused unused

-- | Add a variable as initialised.
addInitVar :: Name -> Static ()
addInitVar name = modify (\s ->
    s { sInitVars = sInitVars s `Set.union` Set.singleton name})

-- | Add a variable as accessed.
addAccessVar :: Name -> Static ()
addAccessVar name = modify (\s ->
    s { sAccessVars = sAccessVars s `Set.union` Set.singleton name })

-- | Prints any static analyis errors.
printErrs :: [StaticErr] -> IO ()
printErrs errs = putStrLn "Static analysis errors:" >> mapM_ printErr errs

printErr :: StaticErr -> IO ()
printErr (Uninit name) = putStrLn $ "Access of uninitialised variable " ++ name
printErr (Unused name) = putStrLn $ "Initialised yet unused variable " ++ name

-- Static analysis of expressions ---------------------------------------------

staticE :: Expr -> Static ()
staticE (Const _)   = return ()
staticE (Add e1 e2) = staticE e1 >> staticE e2
staticE (Sub e1 e2) = staticE e1 >> staticE e2
staticE (Mul e1 e2) = staticE e1 >> staticE e2
staticE (Div e1 e2) = staticE e1 >> staticE e2
staticE (And e1 e2) = staticE e1 >> staticE e2
staticE (Or  e1 e2) = staticE e1 >> staticE e2
staticE (Not e1)    = staticE e1
staticE (Eq  e1 e2) = staticE e1 >> staticE e2
staticE (Gt  e1 e2) = staticE e1 >> staticE e2
staticE (Lt  e1 e2) = staticE e1 >> staticE e2
staticE (Var name)  = do
    initialised <- fmap sInitVars get
    -- Record an unitialised error if that's the case.
    unless (name `Set.member` initialised) $ tell $ Set.singleton $ Uninit name
    -- Record the access.
    addAccessVar name

-- Static analysis of statements ----------------------------------------------

staticS :: Statement -> Static ()
-- Record that the variable must be initialised.
staticS (Assign name e1) = staticE e1 >> addInitVar name
staticS (If e1 s1 s2)    = staticE e1 >> staticS s1 >> staticS s2
staticS (While e1 s1)    = staticE e1 >> staticS s1
staticS (Print e1)       = staticE e1
staticS (Seq s1 s2)      = staticS s1 >> staticS s2
staticS (Try s1 s2)      = staticS s1 >> staticS s2
staticS Pass             = return ()
