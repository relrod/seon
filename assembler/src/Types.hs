{-# LANGUAGE DeriveFunctor #-}
module Types where

import Data.Word

data Instruction =
    NOP
  | ADDA
  | LDA
  | STA
  deriving (Eq, Ord, Show)

data Address =
    Immediate ImmediateAddress
  | Direct DirectAddress
  -- I am going to pretend this doesn't exist for now:
  --- | Indexed
  | Extended ExtendedAddress
  | Inherent
  deriving (Eq, Ord, Show)

data ImmediateAddress =
    Immediate8 Word8
  --- | Immediate16 Word16
  deriving (Eq, Ord, Show)

data DirectAddress =
    DirectAddress Word8
  deriving (Eq, Ord, Show)

data ExtendedAddress =
    ExtendedAddress Word16
  deriving (Eq, Ord, Show)

data AssembleResult a =
    OpCode a Address
  | InvalidAddressingMode
  deriving (Eq, Functor, Ord, Show)

instance Applicative AssembleResult where
  pure a = OpCode a Inherent
  OpCode f _ <*> m = fmap f m
  InvalidAddressingMode <*> _ = InvalidAddressingMode

instance Monad AssembleResult where
  return = pure
  OpCode x _ >>= f = f x
  InvalidAddressingMode >>= _ = InvalidAddressingMode
