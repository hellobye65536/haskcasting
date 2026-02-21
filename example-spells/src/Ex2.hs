{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text.IO qualified as T

import Haskcasting.Compound.Hexcasting (dupN)
import Haskcasting.Fragment (
  Fragment,
  fragmentAsIota,
  (+.+),
 )
import Haskcasting.Iota (IotaVector)
import Haskcasting.Serialize (serializeADefault)

import Haskcasting.Patterns.Hexcasting

explodeLocation :: Fragment (IotaVector ': s) s
explodeLocation =
  dupN @5
    +.+ fragBreakBlock
    +.+ fragCreateWater
    +.+ fragNumericalReflection @10
    +.+ fragExplosion
    +.+ fragConjureBlock
    +.+ fragBreakBlock

main :: IO ()
main = do
  T.putStrLn "\n==== explode location ===="
  mapM_ T.putStrLn $
    serializeADefault $
      fragmentAsIota explodeLocation
