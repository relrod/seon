{-# LANGUAGE DeriveFunctor #-}
module Types where

import Data.Word

data Instruction =
  ABX | ADCA | ADCB | ADDA | ADDB | ADDD | ANDA | ANDB | ANDCC | ASLA |
  ASLB | ASL | ASRA | ASRB | ASR | BITA | BITB | CLRA | CLRB | CLR | CMPA |
  CMPB | CMPD | CMPS | CMPU | CMPX | CMPY | COMA | COMB | COM | CWAI | DAA |
  DECA | DECB | DEC | EORA | EORB | R1 | R2 | INCA | INCB | INC | JMP | JSR |
  LDA | LDB | LDD | LDS | LDU | LDX | LDY | LEAS | LEAU | LEAX | LEAY | LSLA |
  LSLB | LSL | LSRA | LSRB | LSR | MUL | NEGA | NEGB | NEG | NOP | ORA | ORB |
  ORCC | PSHS | PSHU | PULS | PULU | ROLA | ROLB | ROL | RORA | RORB | ROR |
  RTI | RTS | SBCA | SBCB | SEX | STA | STB | STD | STS | STU | STX | STY |
  SUBA | SUBB | SUBD | SWI | SWI2 | SWI3 | SYNC | TFR | TST
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
