{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Hexpose (
  IotaIdentifier (..),
  IotaItem (..),
  IotaDisplay (..),
) where

import Data.Text (Text)
import Haskcasting.ExprLang.TH (mkFragExprInstance)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (Iota (..), IotaNumber)
import Haskcasting.Patterns.Hexcasting

newtype IotaIdentifier = IotaIdentifier Text
instance Iota IotaIdentifier where
  iotaShow (IotaIdentifier tag) = "<identifier: " <> tag <> ">"

newtype IotaItem = IotaItem Text
instance Iota IotaItem where
  iotaShow (IotaItem tag) = "<item: " <> tag <> ">"

newtype IotaDisplay = IotaDisplay Text
instance Iota IotaDisplay where
  iotaShow (IotaDisplay tag) = "<display: " <> tag <> ">"

$( mkFragExprInstance
     "LengthPurification"
     [[t|Fragment '[IotaDisplay] '[IotaNumber]|]]
 )

$( mkFragExprInstance
     "IntegrationDistillation"
     [[t|Fragment '[IotaDisplay, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "DerivationDecomposition"
     [[t|Fragment '[IotaDisplay] '[IotaDisplay, IotaDisplay]|]]
 )

$( mkFragExprInstance
     "AdditiveDistillation"
     [[t|Fragment '[IotaDisplay, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "MultiplicativeDistillation"
     [[t|Fragment '[IotaNumber, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "SelectionDistillation"
     [[t|Fragment '[IotaNumber, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "SurgeonsExaltation"
     [[t|Fragment '[IotaDisplay, IotaNumber, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "ExcisorsDistillation"
     [[t|Fragment '[IotaNumber, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkFragExprInstance
     "ExcisorsDistillation"
     [[t|Fragment '[IotaDisplay] '[IotaDisplay]|]]
 )
