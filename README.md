# Reversible Interpreter

## Files
The bulk of given assignment code is in `Expr.hs`.

Anything relating to `Statement` evaluation is in `Interpreter.hs`.

Static analysis lives in `StaticAnalysis.hs`.

## Running
Two test files are included. `test/fib.txt` which is a working Fibonacci and
`test/fib-errs.txt` which is Fibonacci with static analysis errors.

```bash
stack setup
stack build
stack run -- -- test/fib.txt
```

## Monadic Interpreter (15)
`Main.hs` takes a single command line argument which is a path to a file
containing our `Statement` language. This is `read` and put through static
analysis. If it passes static analysis it is executed in the interpreter. The
first `Statement`, which in both test files is `Seq`, is executed and then the
user is prompted for the next action.

```haskell
-- | Statement evluation monad.
type SEval a = StateT IState (ExceptT SError IO) a

-- | State consists of variable history and the current environment.
data IState = IState { iSHist :: History, iSEnv  :: Env } deriving Show

-- | History of previous `Statment`s and possible variable assignment.
type HistoryItem = (Statement, Maybe (Name, Val))
type History     = [HistoryItem]

-- | A `BackError` is thrown if we want to step back. A `StrError` contains a
-- message for the user.
data SError = BackError Int | StrError String deriving Show
```

## Examine Program Variables (5)
The prompt provides instructions for examining program variables. This means
entering `e` to examine the current **e**nvironment, or entering `i X` to
**i**nspect the value of a single variable `X` and its entire history.

## History (10)
Each `Statement` which has begun execution is recorded. In addition if the
`Statement` was an assignment the value of the variable **before** assignment
is recorded. If the `Statement` is not an assignment `Nothing` is recorded.
The history of the variable can be inspected at the prompt with `i X`.

## Step Backwards (10)
Execution of the `Statement` language can be thought of via a syntax tree.
Consider a `Seq` as a node with two children. Stepping backwards works by
throwing a `BackError` which is caught by the parent node. This means while we
can reverse we can't always directly reverse. If we are executing the left
child of `Seq` then we can step back to the parent `Seq`. However if we're
executing the right child then we can also only step back to the parent `Seq`
and not to the left child, note that assignment history is correctly preserved.

For the case of `While` each iteration is recorded in state history. We can
consider the tree of iterations as a root `While` with a left child as the
while's body and the right child as the `While` guard being evaluated again.
The tree continues extending to the right as long as the `While` guard is
evaluated as true. Because each iteration is recorded we can step back through
the while loop.

If we step backwards to the very beginning we are thusly informed.

## Static Analysis (5)
Static analysis consists of checks for:
- attemping to read an uninitialised variable
- initialising but not using a variable

Static analysis works by recursively working down through the syntax tree of a
`Statement`. Any time a variable is initialised (`Assign`) or accessed (`Var`)
that fact is recorded in the monadic state. Also, whenever a variable is accessed we check if it is in the set of initialised variables, if note we
`tell` an `Uninit` error. Once we have recursed back to the top we check for
any variables that were initialised but not used, see `analyse` below where
`Set.difference` is used.

```haskell
-- | Log any errors and maintain state.
type Static a = WriterT (Set.Set StaticErr) (StateT StaticState IO) a

-- | Looking for uninitialised variable errors and unused variable errors.
data StaticErr = Uninit Name | Unused Name deriving (Eq, Ord)

-- | Track the initialised and accessed variables.
data StaticState = StaticState
    { sInitVars :: Set.Set Name, sAccessVars :: Set.Set Name }

-- Find static analysis errors.
analyse :: Statement -> IO [StaticErr]
analyse stmt = do
    ((_, errs), StaticState initialised accessed) <- runStatic $ staticS stmt
    -- Unused variables are those initialised but not accessed.
    let unused = Set.toList $ initialised `Set.difference` accessed
    return $ Set.toList errs ++ map Unused unused
```
