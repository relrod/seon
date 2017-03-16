module Main where

import Data.ByteString.Builder
import Data.Word
import System.IO

import Types

generateOpcode :: Instruction -> Address -> AssembleResult Word8
generateOpcode NOP  Inherent = OpCode 0x12 Inherent
generateOpcode ADDA a@(Immediate _) = OpCode 0x8b a
generateOpcode LDA  a@(Immediate _) = OpCode 0x86 a
generateOpcode STA  a@(Direct _)    = OpCode 0x86 a
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
