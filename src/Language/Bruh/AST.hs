module Language.Bruh.AST where

import Data.Text (Text)

{-| Variables. |-}
newtype Variable = VVar { vName :: Text }
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
  | EVar     Variable        -- ^ Variable
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
