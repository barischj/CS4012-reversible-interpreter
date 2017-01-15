module Expr where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State    hiding (state)
import qualified Data.Map               as Map

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
