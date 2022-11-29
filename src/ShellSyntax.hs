{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ShellSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

type Name = String -- either the name of a variable or the name of a field

data Var = Name Name
    deriving (Eq, Show)

data Statement
  = Assign Var Expression -- x = e
  | If Expression Block Block -- if e then s1 else s2 end
  | While Expression Block -- while e do s end
  | For Expression Block -- For loop
  | Until Expression Block -- until loop (a lot like repeat)
  deriving (Eq, Show)

data Expression
  = Var Name -- all variables including enviorment vars
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  deriving (Eq, Show)

data Value
  = IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  -- TODO: add arrays here
  deriving (Eq, Show, Ord)

data Uop
  = Not -- `!` :: Bool -> Bool
  | DashZLen  -- Checks if the given string operand size is zero; if it is zero length, then it returns true.
  | DashNLen -- checks if string op size is non-zero if len != 0 then true
  | Str  -- Checks if str is not the empty string; if it is empty, then it returns false.
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `//` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq -- `==, -eq` :: a -> a -> Bool
  | Neq -- `!=, -ne` :: a -> a -> Bool
  | Gt -- `>, -gt`  :: a -> a -> Bool
  | Ge -- `>=, -ge` :: a -> a -> Bool
  | Lt -- `<, -lt`  :: a -> a -> Bool
  | Le -- `<=, -le` :: a -> a -> Bool
  | Concat -- `..` :: String -> String -> String
  | DashO -- logical OR. If one of the operands is true, then the condition becomes true.
  | DashA -- logical AND. If both the operands are true, then the condition becomes true otherwise false.
  deriving (Eq, Show, Enum, Bounded)
