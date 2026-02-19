{-# LANGUAGE OverloadedStrings #-}

module Haskcasting.Serialize.A (
  SerializeOptions (..),
  defaultSerializeOptions,
  Inst (..),
  serializePattern,
  serialize,
) where

import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Pattern (Angle, Direction)

data SerializeOptions = SerializeOptions
  { serOptGreatSpells :: HashMap Text (Direction, [Angle])
  }

defaultSerializeOptions :: SerializeOptions
defaultSerializeOptions = SerializeOptions {serOptGreatSpells = HM.empty}

data Inst
  = Suspend Text
  | Pattern Direction [Angle]
  | MergeN Int
  | Null
  | Bool Bool
  | Number Double
  | Vector Double Double Double
  | String Text

serializePattern :: Direction -> [Angle] -> [Int]
serializePattern dir angles = NE.toList $ NE.reverse $ foldl' go (NE.singleton $ fromEnum dir) angles
 where
  go ls@(d NE.:| _) a = (fromEnum d + fromEnum a) `rem` 6 NE.<| ls

serializeInst :: Inst -> Text
serializeInst = \case
  Suspend tag -> "0;" <> tag
  Pattern dir angles ->
    ("1;" <>) $ fold $ map T.show $ serializePattern dir angles
  MergeN n -> "2;" <> T.show n
  Null -> "3"
  Bool b -> if b then "4" else "5"
  Number n -> "6;" <> showNum n
  Vector x y z -> "7;" <> showNum x <> "," <> showNum y <> "," <> showNum z
  String s -> "8;" <> s
 where
  showNum n =
    if fromIntegral @Int (round n) == n
      then T.show $ (round n :: Int)
      else T.show n

serialize :: SerializeOptions -> Seq Inst -> [Text]
serialize _opt = reverse . foldl' go [] . fmap serializeInst
 where
  go [] s = [s]
  go allouts@(out : outs) s =
    let out' = out <> ";" <> s
     in if T.length out' > maxLength
          then s : allouts
          else out' : outs
  maxLength = 250 :: Int
