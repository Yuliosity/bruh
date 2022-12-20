module Language.Bruh.AST where

import Data.Text (Text)

{-| Variables. |-}
newtype Var = VVar { vName :: Text }
  deriving (Eq, Show)

data UnOp = UNeg
  deriving (Eq, Show)

data BinOp
  = BAdd
  | BSub
  | BMul
  deriving (Eq, Show)

{-| Expressions. |-}
data Expr
  = EInteger Int             -- ^ Integral literal
  | EVar     Var             -- ^ Variable
  | EUnary   UnOp  Expr      -- ^ Unary operator
  | EBinary  BinOp Expr Expr -- ^ Binary operator
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = EInteger . fromInteger
  (+)    = EBinary BAdd
  (-)    = EBinary BSub
  (*)    = EBinary BMul
  negate = EUnary UNeg
  abs    = error "abs is not implemented for Expr"
  signum = error "signum is not implemented for Expr"

data Stmt
  = SAssign Var Expr
  deriving (Eq, Show)

pattern (:=) :: Var -> Expr -> Stmt
pattern v := expr = SAssign v expr
