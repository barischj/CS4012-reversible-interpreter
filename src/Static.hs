module Static where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Set               as Set
import           Lib

-- Looking for uninitialised variable errors and unused variable errors.
data StaticErr = Uninit Name | Unused Name

-- Track the initialised variables.
type StaticState = Set.Set Name

-- Log any errors and maintain state.
type Static a = WriterT [StaticErr] (StateT StaticState Identity) a

-- Add a variable as initialised.
initVar :: Name -> Static ()
initVar name = modify $ Set.union $ Set.singleton name

-- Static analysis of expressions ---------------------------------------------

-- data Expr =
--       Const Val
--     | Add Expr Expr
--     | Sub Expr Expr
--     | Mul Expr Expr
--     | Div Expr Expr
--     | And Expr Expr
--     | Or  Expr Expr
--     | Not Expr
--     | Eq  Expr Expr
--     | Gt  Expr Expr
--     | Lt  Expr Expr
--     | Var Name
--     deriving (Eq, Read, Show)

staticE :: Expr -> Static ()
staticE (Const _)   = return ()
staticE (Add e1 e2) = staticE e1 >> staticE e2
staticE (Var name)  = do
    initialised <- get
    unless (name `Set.member` initialised) $ tell [Uninit name]

-- Static analysis of statements ----------------------------------------------

staticS :: Statement -> Static ()

staticS (Assign name expr) = do
  staticE expr
  initVar name

