{-# LANGUAGE OverloadedStrings #-}

module Haskcasting.Pattern (
  Direction (..),
  directionShow,
  directionParse,
  Angle (..),
  angleShow,
  angleParse,
) where

import Data.String (IsString)
import Data.Text (Text)
import Language.Haskell.TH.Syntax qualified as TH

data Direction
  = DirectionNE
  | DirectionE
  | DirectionSE
  | DirectionSW
  | DirectionW
  | DirectionNW
  deriving (Eq, Bounded, Enum, TH.Lift)

directionShow :: Direction -> Text
directionShow = \case
  DirectionNE -> "NORTH_EAST"
  DirectionE -> "EAST"
  DirectionSE -> "SOUTH_EAST"
  DirectionSW -> "SOUTH_WEST"
  DirectionW -> "WEST"
  DirectionNW -> "NORTH_WEST"

directionParse :: (IsString s, Eq s) => s -> Maybe Direction
directionParse = \case
  "NORTH_EAST" -> Just DirectionNE
  "EAST" -> Just DirectionE
  "SOUTH_EAST" -> Just DirectionSE
  "SOUTH_WEST" -> Just DirectionSW
  "WEST" -> Just DirectionW
  "NORTH_WEST" -> Just DirectionNW
  _ -> Nothing

data Angle
  = AngleW
  | AngleE
  | AngleD
  | AngleS
  | AngleA
  | AngleQ
  deriving (Eq, Bounded, Enum, TH.Lift)

angleShow :: Angle -> Char
angleShow = \case
  AngleW -> 'w'
  AngleE -> 'e'
  AngleD -> 'd'
  AngleS -> 's'
  AngleA -> 'a'
  AngleQ -> 'q'

angleParse :: Char -> Maybe Angle
angleParse = \case
  'w' -> Just AngleW
  'e' -> Just AngleE
  'd' -> Just AngleD
  's' -> Just AngleS
  'a' -> Just AngleA
  'q' -> Just AngleQ
  _ -> Nothing
