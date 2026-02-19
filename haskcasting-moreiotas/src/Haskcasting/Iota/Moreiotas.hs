{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Moreiotas where

import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (Iota (..), IotaNumber)
import Haskcasting.Patterns.Hexcasting (
  FragAdditiveDistillation,
  FragDivisionDistillation,
  FragLengthPurification,
  FragLocatorsDistillation,
  FragMultiplicativeDistillation,
  FragPowerDistillation,
  FragRetrogradePurification,
  FragSelectionExaltation,
 )
import Haskcasting.Serialize.A qualified as SA
import Haskcasting.Patterns.TH (mkFrag)

newtype IotaString = IotaString Text
instance Iota IotaString where
  iotaShow (IotaString s) = "\"" <> s <> "\""
  iotaSerializeA _opt (IotaString s) = Seq.singleton $ SA.String s

newtype IotaMatrix = IotaMatrix [[Double]]
instance Iota IotaMatrix where
  iotaShow _ = "<matrix>"

data IotaItemStack = IotaItemStack Text Int
instance Iota IotaItemStack where
  iotaShow (IotaItemStack item count) =
    "<item stack: "
      <> T.show count
      <> " "
      <> item
      <> ">"

newtype IotaItemType = IotaItemType Text
instance Iota IotaItemType where
  iotaShow (IotaItemType tag) = "<item type: " <> tag <> ">"

newtype IotaEntityType = IotaEntityType Text
instance Iota IotaEntityType where
  iotaShow (IotaEntityType tag) = "<entity type: " <> tag <> ">"

newtype IotaIotaType = IotaIotaType Text
instance Iota IotaIotaType where
  iotaShow (IotaIotaType tag) = "<iota type: " <> tag <> ">"

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
