{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Text.IO qualified as T
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Patterns.Hexcasting

test1 =
  fragMindsReflection
    +.+ fragCompassPurificationII
    +.+ fragMindsReflection
    +.+ fragAlidadesPurification
    +.+ fragAdditiveDistillation @'[IotaVector, IotaVector] @'[IotaVector]
    +.+ fragCast @'[IotaVector] @'[IotaAny]

main :: IO ()
main = do
  T.putStrLn $ iotaShow $ fragmentAsList test1
  T.putStrLn $ iotaShow $ iotaBookkeepersGambit @'[True, False, True]
