{-# LANGUAGE LambdaCase #-}

module Snake.GUI.Keys (
  Key (..),
  keyFromCode
) where

data Key
  = LeftArrow
  | UpArrow
  | RightArrow
  | DownArrow
  | LetterA
  | LetterD
  | LetterI
  | LetterJ
  | LetterK
  | LetterL
  | LetterS
  | LetterW
  | SpaceBar
  deriving (Show, Eq)

keyFromCode :: Int -> Maybe Key
keyFromCode = \case
  32 -> Just SpaceBar
  37 -> Just LeftArrow
  38 -> Just UpArrow
  39 -> Just RightArrow
  40 -> Just DownArrow
  65 -> Just LetterA
  68 -> Just LetterD
  73 -> Just LetterI
  74 -> Just LetterJ
  75 -> Just LetterK
  76 -> Just LetterL
  83 -> Just LetterS
  87 -> Just LetterW
  _ -> Nothing
