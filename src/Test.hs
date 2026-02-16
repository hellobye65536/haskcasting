{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Sequence qualified as Seq
import Data.Text.IO qualified as T
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Patterns.Hexcasting
import Haskcasting.TH (pattern)

dup5 :: Fragment (a ': as) (a ': a ': a ': a ': a ': as)
dup5 =
  Fragment $
    Seq.fromList
      [ iotaCast [pattern| SOUTH_EAST aqaaq |]
      , iotaCast iotaGeminiGambit
      ]

num10 :: Fragment as (IotaNumber ': as)
num10 = Fragment $ Seq.fromList [iotaCast [pattern| SOUTH_EAST aqaae |]]

safeExplosion :: Fragment '[] '[]
safeExplosion =
  fragMindsReflection
    +.+ fragCompassPurificationII
    +.+ dup5
    +.+ fragBreakBlock
    +.+ fragCreateWater
    +.+ num10
    +.+ fragExplosion
    +.+ fragConjureBlock
    +.+ fragBreakBlock

main :: IO ()
main = do
  T.putStrLn $ iotaShow $ fragmentAsList safeExplosion
