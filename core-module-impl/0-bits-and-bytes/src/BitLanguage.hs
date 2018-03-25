{-# LANGUAGE OverloadedStrings #-}

module BitLanguage (BitExpr (..), evalToSteps) where

import Data.Bits ((.&.), (.|.), xor, complement, unsafeShiftL, unsafeShiftR)
import Data.Char (intToDigit)
import Data.Int (Int32)
import Data.List (reverse)
import Data.List.Split (chunksOf)
import Numeric (showIntAtBase)

binaryBase :: Int
binaryBase = 2

numBits :: Int
numBits = 32

bitsSliceSize :: Int
bitsSliceSize = 4

data BitExpr = BitInt Int32
  | BitLeftShift  BitExpr BitExpr
  | BitRightShift BitExpr BitExpr
  | BitNot        BitExpr
  | BitOr         BitExpr BitExpr
  | BitXOr        BitExpr BitExpr
  | BitAnd        BitExpr BitExpr
  deriving Show

bitLangExpr :: BitExpr
bitLangExpr = BitLeftShift (BitAnd (BitInt 32) (BitInt 32)) (BitXOr (BitInt 10) (BitInt 4))

-- Evaluate function of the parsed BitExpression
eval :: BitExpr -> Int32
eval (BitInt e1)           = e1
eval (BitLeftShift e1 e2)  = eval e1 `unsafeShiftL` (fromIntegral $ eval e2)
eval (BitRightShift e1 e2) = eval e1 `unsafeShiftR` (fromIntegral $ eval e2)
eval (BitNot e1)           = complement $ eval e1
eval (BitOr e1 e2)         = eval e1  .|.  eval e2
eval (BitXOr e1 e2)        = eval e1 `xor` eval e2
eval (BitAnd e1 e2)        = eval e1  .&.  eval e2

evalToSteps :: BitExpr -> [String]
evalToSteps baseExpr = Data.List.reverse $ evalToSteps' baseExpr []
  where
    evalToSteps' (BitInt            _) steps = steps
    evalToSteps' (BitLeftShift  e1 e2) steps = let
        leftShiftMsg = constructLeftShiftMessage e1 e2
      in
        leftShiftMsg : (evalToSteps' e2 steps ++ evalToSteps' e1 steps) -- Reversing the list at the end makes it so that e2 is before e1
    evalToSteps' (BitRightShift e1 e2) steps = let
      rightShiftMsg = constructRightShiftMessage e1 e2
      in
        rightShiftMsg : (evalToSteps' e2 steps ++ evalToSteps' e1 steps)
    evalToSteps' (BitNot           e1) steps = constructBitNotMessage e1 : evalToSteps' e1 steps
    evalToSteps' (BitOr         e1 e2) steps = constructBitOrMessage e1 e2 : evalToSteps' e1 steps ++ evalToSteps' e2 steps
    evalToSteps' (BitXOr        e1 e2) steps = constructBitXOrMessage e1 e2 : evalToSteps' e1 steps ++ evalToSteps' e2 steps
    evalToSteps' (BitAnd        e1 e2) steps = constructBitAndMessage e1 e2 : evalToSteps' e1 steps ++ evalToSteps' e2 steps

constructLeftShiftMessage :: BitExpr -> BitExpr -> String
constructLeftShiftMessage e1 e2 = let
      (evaledE1, evaledE2) = (eval e1, eval e2)
      binaryE1 = convertInt32ToShowableBinary evaledE1
      strE1 = show evaledE1
      strE2 = show evaledE2
      evaledE1E2 = eval $ BitLeftShift e1 e2
    in
      unlines [
        "",
        "Performing bitwise left shift on " ++ strE1 ++ " << " ++ strE2,
        "",
        strE1 ++ " in binary is " ++ binaryE1 ++ ".",
        "",
        "Performing " ++ binaryE1 ++ " << " ++ strE2,
        "",
        "After shifting left by " ++ strE2 ++ " bits, the result from left shift is:",
        "",
        convertInt32ToShowableBinary evaledE1E2,
        "",
        "Which in base 10 is equal to " ++ show evaledE1E2 ++ "."
      ]

constructRightShiftMessage :: BitExpr -> BitExpr -> String
constructRightShiftMessage e1 e2 = let
      (evaledE1, evaledE2) = (eval e1, eval e2)
      binaryE1 = convertInt32ToShowableBinary evaledE1
      strE1 = show evaledE1
      strE2 = show evaledE2
      evaledE1E2 = eval $ BitRightShift e1 e2
    in
      unlines [
        "",
        "Performing bitwise right shift on " ++ strE1 ++ " >> " ++ strE2,
        "",
        strE1 ++ " in binary is " ++ binaryE1 ++ ".",
        "",
        "Performing " ++ binaryE1 ++ " >> " ++ strE2,
        "",
        "After shifting right by " ++ strE2 ++ " bits, the result from right shift is:",
        "",
        convertInt32ToShowableBinary evaledE1E2,
        "",
        "Which in base 10 is equal to " ++ show evaledE1E2 ++ "."
      ]

constructBitNotMessage :: BitExpr -> String
constructBitNotMessage e1 = let
      evaledE1 = eval e1
      strEvaledE1 = show evaledE1
      evaledNot = eval $ BitNot e1
      strEvaledNot = show evaledNot
    in
      unlines [
        "",
        "Performing bitwise not (~) on " ++ strEvaledE1,
        "",
        strEvaledE1 ++ " in binary is " ++ convertInt32ToShowableBinary evaledE1 ++ ".",
        "",
        "Flipping all of the bits, we get " ++ convertInt32ToShowableBinary evaledNot ++ ".",
        "",
        "Which in base 10 is equal to " ++ strEvaledNot ++ "."
      ]

constructBitOrMessage :: BitExpr -> BitExpr -> String
constructBitOrMessage e1 e2 = let
    (evaledE1, evaledE2) = (eval e1, eval e2)
    (strEvaledE1, strEvaledE2) = (show evaledE1, show evaledE2)
    (binaryE1, binaryE2) = (convertInt32ToShowableBinary evaledE1, convertInt32ToShowableBinary evaledE2)
    evaledOr = eval $ BitOr e1 e2
    in
      unlines [
        "",
        "Performing bitwise or on " ++ strEvaledE1 ++ " | " ++ strEvaledE2,
        "",
        strEvaledE1 ++ " in binary is " ++ binaryE1 ++ ".",
        "",
        strEvaledE2 ++ " in binary is " ++ binaryE2 ++ ".",
        "",
        "Oring the two:",
        "",
        binaryE1,
        binaryE2 ++ " | ",
        replicate (numBits + (numBits `quot` bitsSliceSize) - 1) '-',
        convertInt32ToShowableBinary evaledOr,
        "",
        "Which in base 10 is equal to " ++ show evaledOr ++ "."
      ]

constructBitXOrMessage :: BitExpr -> BitExpr -> String
constructBitXOrMessage e1 e2 = let
      (evaledE1, evaledE2) = (eval e1, eval e2)
      (strEvaledE1, strEvaledE2) = (show evaledE1, show evaledE2)
      (binaryE1, binaryE2) = (convertInt32ToShowableBinary evaledE1, convertInt32ToShowableBinary evaledE2)
      evaledXOr = eval $ BitXOr e1 e2
    in
      unlines [
        "",
        "Performing bitwise XOR on " ++ strEvaledE1 ++ " ^ " ++ strEvaledE2,
        "",
        strEvaledE1 ++ " in binary is " ++ binaryE1 ++ ".",
        "",
        strEvaledE2 ++ " in binary is " ++ binaryE2 ++ ".",
        "",
        "XORing the two:",
        "",
        binaryE1,
        binaryE2 ++ " ^ ",
        replicate (numBits + (numBits `quot` bitsSliceSize) - 1) '-',
        convertInt32ToShowableBinary evaledXOr,
        "",
        "Which in base 10 is equal to " ++ show evaledXOr ++ "."
      ]

constructBitAndMessage :: BitExpr -> BitExpr -> String
constructBitAndMessage e1 e2 = let
      (evaledE1, evaledE2) = (eval e1, eval e2)
      (strEvaledE1, strEvaledE2) = (show evaledE1, show evaledE2)
      (binaryE1, binaryE2) = (convertInt32ToShowableBinary evaledE1, convertInt32ToShowableBinary evaledE2)
      evaledAnd = eval $ BitAnd e1 e2
    in
      unlines [
        "",
        "Performing bitwise and on " ++ strEvaledE1 ++ " & " ++ strEvaledE2,
        "",
        strEvaledE1 ++ " in binary is " ++ binaryE1 ++ ".",
        "",
        strEvaledE2 ++ " in binary is " ++ binaryE2 ++ ".",
        "",
        "Anding the two:",
        "",
        binaryE1,
        binaryE2 ++ " & ",
        replicate (numBits + (numBits `quot` bitsSliceSize) - 1) '-',
        convertInt32ToShowableBinary evaledAnd,
        "",
        "Which in base 10 is equal to " ++ show evaledAnd ++ "."
      ]

-- This function is sort of a hack. 'showIntAtBase' is not happy with negative numbers, so we'll
-- instead take the complement of the int to yield a positive number, then manually flip the bits
-- to get the negative number representation and pad it with either 0s or 1s depending on if it's positive or negative.
convertInt32ToShowableBinary :: Int32 -> String
convertInt32ToShowableBinary int
  | int >= 0 = convertInt32ToShowableBinary' (fromIntegral int) '0' id
  | int <  0 = convertInt32ToShowableBinary' (fromIntegral $ complement int) '1' flipBit
  where
    convertInt32ToShowableBinary' int' paddedChar bitFlipper = let
      result = bitFlipper <$> showIntAtBase binaryBase intToDigit int' ""
      paddedChars = replicate (numBits - length result) paddedChar
      chunkedResult = chunksOf bitsSliceSize $ paddedChars ++ result
      in
        unwords chunkedResult

    flipBit '0' = '1'
    flipBit '1' = '0'
