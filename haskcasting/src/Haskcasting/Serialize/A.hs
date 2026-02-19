{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Haskcasting.Pattern (Angle, Pattern, angles, patternAngles)
import Haskcasting.Pattern qualified as P

data SerializeOptions = SerializeOptions
  { serOptGreatSpells :: HashMap Text Pattern
  , serOptPatternIntrinsics :: Bool
  }

defaultSerializeOptions :: SerializeOptions
defaultSerializeOptions =
  SerializeOptions
    { serOptGreatSpells = HM.empty
    , serOptPatternIntrinsics = True
    }

data Inst
  = Suspend Text
  | Pattern Pattern
  | MergeN Int
  | Null
  | Bool Bool
  | Number Double
  | Vector Double Double Double
  | String Text
  | PatternConsideration
  | PatternIntrospection
  | PatternRetrospection

serializePattern :: Pattern -> [Int]
serializePattern (P.Pattern dir ang) = NE.toList $ NE.reverse $ foldl' go (NE.singleton $ fromEnum dir) ang
 where
  go ls@(d NE.:| _) a = (fromEnum d + fromEnum a) `rem` 6 NE.<| ls

intrinsicPatterns :: [([Angle], Inst)]
intrinsicPatterns =
  [ ([angles| qqqaw |], PatternConsideration)
  , ([angles| qqq |], PatternIntrospection)
  , ([angles| eee |], PatternRetrospection)
  ]

serializeInst :: SerializeOptions -> Inst -> Text
serializeInst
  opt@SerializeOptions
    { serOptPatternIntrinsics = patIntrs
    } =
    \case
      Suspend tag -> "0;" <> tag
      Pattern pat ->
        if
          | patIntrs
          , Just inst <- lookup (patternAngles pat) intrinsicPatterns ->
              serializeInst opt inst
          | otherwise -> ("1;" <>) $ fold $ map T.show $ serializePattern pat
      MergeN n -> "2;" <> T.show n
      Null -> "3"
      Bool b -> if b then "4" else "5"
      Number n -> "6;" <> showNum n
      Vector x y z -> "7;" <> showNum x <> "," <> showNum y <> "," <> showNum z
      String s -> "8;" <> s
      PatternConsideration -> "9"
      PatternIntrospection -> "10"
      PatternRetrospection -> "11"
   where
    showNum n =
      if fromIntegral @Int (round n) == n
        then T.show $ (round n :: Int)
        else T.show n

serialize :: SerializeOptions -> Seq Inst -> [Text]
serialize opt = reverse . foldl' go [] . fmap (serializeInst opt)
 where
  go [] s = [s]
  go allouts@(out : outs) s =
    let out' = out <> ";" <> s
     in if T.length out' > maxLength
          then s : allouts
          else out' : outs
  maxLength = 250 :: Int
