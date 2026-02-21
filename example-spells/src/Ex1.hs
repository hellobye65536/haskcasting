{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text.IO qualified as T

import Haskcasting.Fragment (
  Fragment,
  fragmentAsIota,
  (+.+),
 )
import Haskcasting.Serialize (serializeADefault)

import Haskcasting.Patterns.Hexcasting

breakBlock :: Fragment cs cs
breakBlock =
  fragMindsReflection
    +.+ fragCompassPurification
    +.+ fragMindsReflection
    +.+ fragAlidadesPurification
    +.+ fragArchersDistillation
    +.+ fragBreakBlock

main :: IO ()
main = do
  T.putStrLn "\n==== break block ===="
  mapM_ T.putStrLn $
    serializeADefault $
      fragmentAsIota breakBlock
