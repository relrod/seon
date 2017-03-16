module Main where

import Data.ByteString.Builder
import Data.Word
import System.IO

import Types

generateOpcode :: Instruction -> Address -> AssembleResult Word8
generateOpcode ABX  Inherent        = OpCode 0x3a Inherent
generateOpcode ADCA a@(Immediate _) = OpCode 0x89 a
generateOpcode ADCA a@(Direct _)    = OpCode 0x99 a
--generateOpcode ADCA a@(Indexed _)   = OpCode 0xa9 a
generateOpcode ADCA a@(Extended _)  = OpCode 0xb9 a
generateOpcode ADCB a@(Immediate _) = OpCode 0xc9 a
generateOpcode ADCB a@(Direct _)    = OpCode 0xd9 a
--generateOpcode ADCB a@(Indexed _)   = OpCode 0xe9 a
generateOpcode ADCB a@(Extended _)  = OpCode 0xf9 a
generateOpcode ADDA a@(Immediate _) = OpCode 0x8b a
generateOpcode ADDA a@(Direct _)    = OpCode 0x9b a
--generateOpcode ADDA a@(Indexed _)   = OpCode 0xab a
generateOpcode ADDA a@(Extended _)  = OpCode 0xbb a
generateOpcode NOP  Inherent        = OpCode 0x12 Inherent
generateOpcode LDA  a@(Immediate _) = OpCode 0x86 a
generateOpcode STA  a@(Direct _)    = OpCode 0x97 a
generateOpcode _    _               = InvalidAddressingMode

-- TODO: How the hell does Word16 fit in here
-- in extended and Immediate16???
assembleToWord8 :: AssembleResult Word8 -> [Word8]
assembleToWord8 (OpCode w a) = [w, destruct a]
  where
    destruct (Immediate (Immediate8 a)) = a
    destruct (Direct (DirectAddress a)) = a

getValidOpcodes :: [AssembleResult a] -> [AssembleResult a]
getValidOpcodes x = [OpCode y a | OpCode y a <- x]

-- 56 + 43 = 99 (0x63)
program =
  [ generateOpcode LDA  (Immediate (Immediate8 0x38))
  , generateOpcode ADDA (Immediate (Immediate8 0x2b))
  , generateOpcode STA  (Direct (DirectAddress 0x42))
  ]

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetBinaryMode stdout True
  if InvalidAddressingMode `elem` program
    then error "Invalid addressing mode found. :("
    else mapM_ (hPutBuilder stdout . word8) (getValidOpcodes program >>= assembleToWord8)
