{-# LANGUAGE OverloadedStrings #-}

module Haskcasting.Serialize.A (Inst (..), MonadSerialize (..), serialize) where

import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Pattern (Angle, Direction)

class Monad m => MonadSerialize m where
  findGreatPattern :: Text -> m (Maybe (Direction, [Angle]))

instance MonadSerialize Identity where
  findGreatPattern _ = pure Nothing

data Inst
  = Suspend Text
  | Pattern Direction [Angle]
  | EmptyList
  | Push
  | Null
  | Bool Bool
  | Number Double
  | Vector Double Double Double
  | String Text

serializeInst :: Inst -> Text
serializeInst = \case
  Suspend tag -> "0;" <> tag
  Pattern dir angles ->
    ("1;" <>) $ fold $ fmap T.show $ NE.reverse $ foldl' go (NE.singleton $ fromEnum dir) angles
   where
    go ls@(d NE.:| _) a = (fromEnum d + fromEnum a) `rem` 6 NE.<| ls
  EmptyList -> "2"
  Push -> "3"
  Null -> "4"
  Bool b -> if b then "5" else "6"
  Number n -> "7;" <> showNum n
  Vector x y z -> "8;" <> showNum x <> "," <> showNum y <> "," <> showNum z
  String s -> "9;" <> s
 where
  showNum n =
    if fromIntegral @Int (round n) == n
      then T.show $ (round n :: Int)
      else T.show n

serialize :: Seq Inst -> [Text]
serialize = reverse . foldl' go [] . fmap serializeInst
 where
  go [] s = [s]
  go allouts@(out : outs) s =
    let out' = out <> ";" <> s
     in if T.length out' > maxLength
          then s : allouts
          else out' : outs
  maxLength = 250 :: Int
