module Main where

import Data.Text.IO qualified as T
import Haskcasting.Compound.Hexcasting (dupN)
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Patterns.Hexcasting

safeExplosion :: Fragment '[] '[]
safeExplosion =
  fragMindsReflection
    +.+ fragCompassPurificationII
    +.+ dupN @5
    +.+ fragBreakBlock
    +.+ fragCreateWater
    +.+ fragNumericalReflection @10
    +.+ fragExplosion
    +.+ fragConjureBlock
    +.+ fragBreakBlock

main :: IO ()
main = do
  T.putStrLn $ iotaShow $ fragmentAsList safeExplosion
