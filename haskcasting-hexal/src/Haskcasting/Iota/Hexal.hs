{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Hexal where

import Data.Text (Text)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (Iota (..), IotaNumber)
import Haskcasting.Iota.Moreiotas (IotaItemType)
import Haskcasting.Patterns.Hexcasting (FragAdditiveDistillation, FragLengthPurification)
import Haskcasting.Patterns.Moreiotas (FragSortersPurification)
import Haskcasting.Patterns.TH (mkFrag)

newtype IotaMote = IotaMote Text deriving (Eq)
instance Iota IotaMote where
  iotaShow (IotaMote t) = "<mote: " <> t <> ">"

newtype IotaGate = IotaGate Text deriving (Eq)
instance Iota IotaGate where
  iotaShow (IotaGate t) = "<gate: " <> t <> ">"

-- overloads

$( mkFrag
     "AdditiveDistillation"
     [[t|Fragment '[IotaMote, IotaMote] '[IotaMote]|]]
 )

$( mkFrag
     "LengthPurification"
     [[t|Fragment '[IotaMote] '[IotaNumber]|]]
 )

$( mkFrag
     "SortersPurification"
     [[t|Fragment '[IotaMote] '[IotaItemType]|]]
 )
