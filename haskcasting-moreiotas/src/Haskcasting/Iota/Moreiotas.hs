{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Moreiotas where

import Data.Text (Text)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (Iota (..), IotaNumber)
import Haskcasting.Patterns.Hexcasting (FragAdditiveDistillation, FragDivisionDistillation, FragLengthPurification, FragLocatorsDistillation, FragMultiplicativeDistillation, FragPowerDistillation, FragRetrogradePurification, FragSelectionExaltation)
import Haskcasting.TH (mkFrag)

newtype IotaString = IotaString Text
instance Iota IotaString where
  iotaShow (IotaString s) = "\"" <> s <> "\""

newtype IotaMatrix = IotaMatrix [[Double]]
instance Iota IotaMatrix where
  iotaShow _ = "<matrix>"

data IotaItemStack = IotaItemStack Text Int
instance Iota IotaItemStack where
  iotaShow _ = "<item stack>"

newtype IotaItemType = IotaItemType Text
instance Iota IotaItemType where
  iotaShow _ = "<item type>"

newtype IotaEntityType = IotaEntityType Text
instance Iota IotaEntityType where
  iotaShow _ = "<entity type>"

newtype IotaIotaType = IotaIotaType Text
instance Iota IotaIotaType where
  iotaShow _ = "<iota type>"

-- overloads

$( mkFrag
     "AdditiveDistillation"
     [ [t|Fragment '[IotaString, IotaString] '[IotaString]|]
     , [t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]
     ]
 )

$( mkFrag
     "MultiplicativeDistillation"
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFrag
     "DivisionDistillation"
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFrag
     "LengthPurification"
     [ [t|Fragment '[IotaString] '[IotaNumber]|]
     , [t|Fragment '[IotaItemStack] '[IotaNumber]|]
     ]
 )

$( mkFrag
     "PowerDistillation"
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFrag
     "LocatorsDistillation"
     [[t|Fragment '[IotaString, IotaString] '[IotaNumber]|]]
 )

$( mkFrag
     "RetrogradePurification"
     [[t|Fragment '[IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFrag
     "SelectionExaltation"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaString] '[IotaString]|]]
 )
