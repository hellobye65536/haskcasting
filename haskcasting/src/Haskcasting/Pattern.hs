{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Pattern (
  Direction (..),
  directionShow,
  directionParse,
  Angle (..),
  angleShow,
  angleParse,
  Pattern (..),
  patternDirection,
  patternAngles,
  patternShow,
  -- template haskell
  angles,
  pattern,
) where

import Data.List (dropWhileEnd)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T

import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Exp (AppE, ConE), Quote, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))

data Direction
  = DirectionNE
  | DirectionE
  | DirectionSE
  | DirectionSW
  | DirectionW
  | DirectionNW
  deriving (Eq, Bounded, Enum, Lift)

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
  deriving (Eq, Bounded, Enum, Lift)

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

data Pattern = Pattern Direction [Angle]
  deriving (Eq, Lift)

patternDirection :: Pattern -> Direction
patternDirection (Pattern dir _) = dir

patternAngles :: Pattern -> [Angle]
patternAngles (Pattern _ ang) = ang

patternShow :: Pattern -> Text
patternShow (Pattern dir ang) =
  "HexPattern["
    <> directionShow dir
    <> ", "
    <> (T.pack $ map angleShow ang)
    <> "]"

-- template haskell

isAsciiWhitespace :: Char -> Bool
isAsciiWhitespace c = '\9' <= c && c <= '\13' || c == ' '

parseAngles :: Quote m => String -> m Exp
parseAngles xs = listE $ concatMap go xs
 where
  go c
    | isAsciiWhitespace c = []
    | Just ident <- angleParse $ toLower c = [lift ident]
    | otherwise = error $ "invalid char '" <> [c] <> "'"

parseDirection :: Quote m => String -> m Exp
parseDirection x = lift $ fromMaybe err $ directionParse $ map toUpper x'
 where
  x' = dropWhileEnd isAsciiWhitespace $ dropWhile isAsciiWhitespace x
  err = error $ "invalid direction: '" <> x' <> "'"

parsePattern :: Quote m => String -> m Exp
parsePattern x = apply (ConE 'Pattern) <$> sequenceA [parseDirection dir, parseAngles ang]
 where
  apply fn = foldl AppE fn
  x' = dropWhile isAsciiWhitespace x
  (dir, ang) = break isAsciiWhitespace x'

angles :: QuasiQuoter
angles =
  QuasiQuoter {quoteDec = err "declaration", quoteType = err "type", quoteExp = parseAngles, quotePat = err "pattern"}
 where
  err p = error $ "bad position: " <> p

pattern :: QuasiQuoter
pattern =
  QuasiQuoter {quoteDec = err "declaration", quoteType = err "type", quoteExp = parsePattern, quotePat = err "pattern"}
 where
  err p = error $ "bad position: " <> p
